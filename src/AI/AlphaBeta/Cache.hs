{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module AI.AlphaBeta.Cache where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.ReadWriteLock as RWL
import Control.Monad.Catch (bracket_)
import qualified Control.Monad.Metrics as Metrics
import qualified Data.Map as M
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import Data.Maybe
import Data.Binary
import Data.Store
import Data.Typeable
import Data.Default
import GHC.Generics
import System.FilePath
import System.Environment
import System.Directory
import System.IO
import System.Posix.Types
import System.Posix.IO
import System.Clock

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Parallel
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent

loadAiCache :: GameRules rules
            => (ScoreMoveInput rules -> Checkers (Move, Score))
            -> AlphaBeta rules
            -> Checkers (AICacheHandle rules)
loadAiCache scoreMove (AlphaBeta params rules) = do
  let getKey (ai, handle, side, depth, board, move) = move
  processor <- runProcessor (abThreads params) getKey scoreMove
  cache <- liftIO $ atomically $ newTVar $ AICache False processor emptyBoardMap
  cachePath <- do
              home <- liftIO $ getEnv "HOME"
              let directory = home </> ".cache" </> "hcheckers" </> rulesName rules </> "ai.cache"
              liftIO $ createDirectoryIfMissing True directory
              return directory
  let indexPath = cachePath </> "index"
      dataPath = cachePath </> "data"
            
  writeQueue <- liftIO $ atomically $ newTChan
  cleanupQueue <- liftIO $ atomically $ newTVar PQ.empty
  let mbMode
        | abSaveCache params = Just ReadWrite
        | abLoadCache params = Just ReadOnly
        | otherwise = Nothing
  (indexFile, dataFile, exist) <- case mbMode of
                    Nothing -> return (Nothing, Nothing, False)
                    Just mode -> do
                      indexExists <- liftIO $ doesFileExist indexPath
                      dataExists <- liftIO $ doesFileExist dataPath
                      let exist = dataExists && indexExists
                      if abLoadCache params && not (abSaveCache params) && not exist
                        then return (Nothing, Nothing, exist)
                        else liftIO $ do
                          let fileMode = Just 0o644
                          let flags = defaultFileFlags
                          indexFd <- openFd indexPath mode fileMode flags
                          dataFd <- openFd dataPath mode fileMode flags
                          return (Just indexFd, Just dataFd, exist)
  when (isJust mbMode) $
      liftIO $ putStrLn $ "Opened: " ++ cachePath

  st <- ask
  indexLock <- liftIO RWL.new
  dataLock <- liftIO RWL.new
  indexBlockLocks <- liftIO $ atomically $ newTVar M.empty
  dataBlockLocks <- liftIO $ atomically $ newTVar M.empty

  let indexHandle = case indexFile of
        Nothing -> Nothing
        Just fd -> Just $ FHandle {
                     fhOffset = 0,
                     fhHandle = fd,
                     fhLocks = Locks {
                         lBlocksCount = indexLock,
                         lBlockLocks = indexBlockLocks
                     }
                   }
  let dataHandle = case dataFile of
        Nothing -> Nothing
        Just fd -> Just $ FHandle {
                     fhOffset = 0,
                     fhHandle = fd,
                     fhLocks = Locks {
                         lBlocksCount = dataLock,
                         lBlockLocks = dataBlockLocks
                     }
                   }
  let handle = AICacheHandle {
      aichRules = rules,
      aichData = cache,
      aichWriteQueue = writeQueue,
      aichCleanupQueue = cleanupQueue,
      aichIndexFile = indexHandle,
      aichDataFile = dataHandle
    }
  when (abSaveCache params && isJust indexFile && not exist) $ do
     runStorage handle $ initFile
  when (abSaveCache params) $ do
    liftIO $ forkIO $
      runCheckersT (cacheDumper rules params handle) st
    return ()
  liftIO $ forkIO $
      runCheckersT (cacheCleaner handle) st

  return handle

cacheDumper :: GameRules rules => rules -> AlphaBetaParams -> AICacheHandle rules -> Checkers ()
cacheDumper rules params handle =
  when (abSaveCache params) $ forever $ do
    repeatTimed "write" 30 $ do
      -- threadDelay $ 100*1000
      mbRecord <- liftIO $ atomically $ checkWriteQueue (aichWriteQueue handle)
      case mbRecord of
        Nothing -> return False
        Just (board, depth, side, value) -> do
          Metrics.increment "cache.records.writen"
          runStorage handle $
              putRecordFile board depth side value
          return True
      
    liftIO $ threadDelay $ 30 * 1000 * 1000

cacheCleaner :: AICacheHandle rules -> Checkers ()
cacheCleaner handle = forever $ do
    repeatTimed "cleanup" 5 $ do
      now <- liftIO $ getTime Monotonic
      mbRecord <- liftIO $ atomically $ checkCleanupQueue (aichCleanupQueue handle) now
      case mbRecord of
        Nothing -> return False
        Just (bc, bk) -> do
          liftIO $ atomically $ do
            aic <- readTVar (aichData handle)
            let cache = aicData aic
            let aic' = aic {aicData = deleteBoardMap bc bk cache}
            writeTVar (aichData handle) aic'
          Metrics.increment "cache.records.cleaned"
          return True
    liftIO $ threadDelay $ 30 * 1000 * 1000

normalize :: BoardSize -> (BoardCounts,BoardKey,Side) -> (BoardCounts,BoardKey,Side)
normalize bsize (bc,bk,side) =
  let bk' = flipBoardKey bsize bk
      bc' = flipBoardCounts bc
  in  if bc' < bc
        then (bc', bk', opposite side)
        else (bc, bk, side)

lookupAiCache :: GameRules rules => AlphaBetaParams -> Board -> Int -> Side -> AICacheHandle rules -> Checkers (Maybe CacheItemSide)
lookupAiCache params board depth side handle = do
    let bsize = boardSize (aichRules handle)
    let (bc, bk, side') = normalize bsize (boardCounts board, boardKey board, side)
    result <- lookupMemory (bc,bk) side
    case result of
      Just _ -> do
        Metrics.increment "cache.hit.memory"
        queueCleanup bc bk
        return result
      Nothing -> do
        mbValue <- runStorage handle $ event "file lookup" $ lookupFile bk depth side'
        case mbValue of
          Nothing -> do
            Metrics.increment "cache.miss"
            return Nothing
          Just value -> do
            Metrics.increment "cache.hit.file"
            putAiCache' params (bc,bk) depth side' value handle
            return mbValue

  where 
    queueCleanup bc bk = do
      let key = (bc, bk)
      now <- liftIO $ getTime Monotonic
      liftIO $ atomically $ putCleanupQueue (aichCleanupQueue handle) key now
    
    lookupMemory (bc, bk) side = Metrics.timed "cache.lookup.memory" $ do
      let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
      if total <= abUseCacheMaxPieces params && depth <= abUseCacheMaxPieces params
        then do
            AICache _ _ cache <- liftIO $ atomically $ readTVar (aichData handle)
            case lookupBoardMap (bc,bk) cache of
              Nothing -> return Nothing
              Just byDepth -> do
                let depths = [depth .. depth + abUseCacheMaxDepthPlus params] ++
                             [depth - abUseCacheMaxDepthMinus params .. depth-1] 
                case foldl mplus Nothing [M.lookup d byDepth | d <- depths ] of
                  Nothing -> return Nothing
                  Just item -> case side of
                                 First -> return $ ciFirst item
                                 Second -> return $ ciSecond item
        else return Nothing

putAiCache' :: GameRules rules => AlphaBetaParams -> (BoardCounts,BoardKey) -> Int -> Side -> StorageValue -> AICacheHandle rules -> Checkers ()
putAiCache' params (bc,bk) depth side sideItem handle = do
  let bsize = boardSize (aichRules handle)
  let (bc', bk', side') = normalize bsize (bc, bk, side)
  let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
  when (total <= abUpdateCacheMaxPieces params && depth > abUpdateCacheMaxDepth params) $ Metrics.timed "cache.put.memory" $ do
      now <- liftIO $ getTime Monotonic
      Metrics.increment "cache.records.put"
      liftIO $ atomically $ do
        aic <- readTVar (aichData handle)
        let updateItem item1 item2 =
              case side' of
                First -> item1 {ciFirst = ciFirst item2 `mplus` ciFirst item1}
                Second -> item2 {ciSecond = ciSecond item2 `mplus` ciSecond item1}

            updateDepthMap m1 m2 = M.unionWith updateItem m1 m2

            item = case side' of
                     First -> CacheItem {ciFirst = Just sideItem, ciSecond = Nothing}
                     Second -> CacheItem {ciFirst = Nothing, ciSecond = Just sideItem}

            init = M.singleton depth item

            newAicData = putBoardMapWith updateDepthMap (bc',bk') init (aicData aic)
            aic' = aic {aicDirty = True, aicData = newAicData}

            Just perBoard = lookupBoardMap (bc',bk') newAicData 

        writeTVar (aichData handle) aic'
        putWriteQueue (aichWriteQueue handle) (bk', depth, side', sideItem)
        putCleanupQueue (aichCleanupQueue handle) (bc', bk') now


putAiCache :: GameRules rules => AlphaBetaParams -> Board -> Int -> Side -> Score -> [Move] -> AICacheHandle rules -> Checkers ()
putAiCache params board depth side score moves handle = do
  let sideItem = CacheItemSide {cisScore = score}
  putAiCache' params (boardCounts board, boardKey board) depth side sideItem handle

