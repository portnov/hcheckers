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
import Control.Monad.Catch (bracket_)
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
              let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
              liftIO $ createDirectoryIfMissing True directory
              return $ directory </> "ai.cache"
            
  writeQueue <- liftIO $ atomically $ newTChan
  cleanupQueue <- liftIO $ atomically $ newTVar PQ.empty
  let mbMode
        | abSaveCache params = Just ReadWrite
        | abLoadCache params = Just ReadOnly
        | otherwise = Nothing
  (file, exist) <- case mbMode of
                    Nothing -> return (Nothing, False)
                    Just mode -> do
                      exist <- liftIO $ doesFileExist cachePath
                      if abLoadCache params && not (abSaveCache params) && not exist
                        then return (Nothing, exist)
                        else liftIO $ do
                          let fileMode = Just 0o644
                          let flags = defaultFileFlags
                          fd <- openFd cachePath mode fileMode flags
                          return (Just fd, exist)
  liftIO $ putStrLn $ "Opened: " ++ cachePath
  st <- ask
  boardLock <- liftIO $ newQSem 1
  bcLock <- liftIO $ newQSem 1
  lockedBoard <- liftIO $ atomically $ newTVar Nothing
  let handle = AICacheHandle {
      aichData = cache,
      aichWriteQueue = writeQueue,
      aichCleanupQueue = cleanupQueue,
      aichBlocksCountLock = bcLock,
      aichLockedBoard = lockedBoard,
      aichBoardLock = boardLock,
      aichFileHandle = file
    }
  when (abSaveCache params && isJust file && not exist) $ do
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
    repeatTimed 30 $ do
      -- threadDelay $ 100*1000
      mbRecord <- liftIO $ atomically $ checkWriteQueue (aichWriteQueue handle)
      case mbRecord of
        Nothing -> do
          liftIO $ putStrLn "Write queue exhaused"
          return False
        Just (board, depth, side, value) -> do
          runStorage handle $
              putRecordFile board depth side value
          return True
      
    liftIO $ threadDelay $ 30 * 1000 * 1000

cacheCleaner :: AICacheHandle rules -> Checkers ()
cacheCleaner handle = forever $ do
    repeatTimed 5 $ do
      now <- liftIO $ getTime Monotonic
      mbRecord <- liftIO $ atomically $ checkCleanupQueue (aichCleanupQueue handle) now
      case mbRecord of
        Nothing -> do
          liftIO $ putStrLn "Cleanup queue exhaused"
          return False
        Just (bc, bk) -> do
          liftIO $ atomically $ do
            aic <- readTVar (aichData handle)
            let cache = aicData aic
            let aic' = aic {aicData = deleteBoardMap bc bk cache}
            writeTVar (aichData handle) aic'
          return True
    liftIO $ threadDelay $ 30 * 1000 * 1000

-- saveAiCache :: GameRules rules => rules -> AlphaBetaParams -> AICacheHandle rules -> Checkers Bool
-- saveAiCache rules params handle = do
--       AICache dirty _ cache <- liftIO $ atomically $ readTVar (aichData handle)
--       if dirty
--         then do
--           home <- liftIO $ getEnv "HOME"
--           let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
--           liftIO $ createDirectoryIfMissing True directory
--           let path = directory </> "ai.cache"
--           timed "Saving cache" $ liftIO $ B.writeFile path $ Data.Store.encode cache
--           liftIO $ atomically $ modifyTVar (aichData handle) $ \aic -> aic {aicDirty = False}
--           return True
--         else return False

lookupAiCache :: AlphaBetaParams -> Board -> Int -> Side -> AICacheHandle rules -> Checkers (Maybe CacheItemSide)
lookupAiCache params board depth side handle = do
    result <- lookupMemory
    case result of
      Just _ -> do
        queueCleanup
        return result
      Nothing -> do
        mbValue <- runStorage handle $ event "file lookup" $ lookupFile board depth side
        case mbValue of
          Nothing -> return Nothing
          Just value -> do
            putAiCache' params board depth side value handle
            return mbValue

  where 
    queueCleanup = do
      let key = (boardCounts board, boardKey board)
      now <- liftIO $ getTime Monotonic
      liftIO $ atomically $ putCleanupQueue (aichCleanupQueue handle) key now
    
    lookupMemory = event "memory lookup" $ do
      let c = boardCounts board
          total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
      if total <= abUseCacheMaxPieces params && depth <= abUseCacheMaxPieces params
        then do
            AICache _ _ cache <- liftIO $ atomically $ readTVar (aichData handle)
            case lookupBoardMap board cache of
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

putAiCache' :: AlphaBetaParams -> Board -> Int -> Side -> StorageValue -> AICacheHandle rules -> Checkers ()
putAiCache' params board depth side sideItem handle = do
  let c = boardCounts board
      total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
  when (total <= abUpdateCacheMaxPieces params && depth > abUpdateCacheMaxDepth params) $ do
      now <- liftIO $ getTime Monotonic
      liftIO $ atomically $ do
        aic <- readTVar (aichData handle)
        let updateItem item1 item2 =
              case side of
                First -> item1 {ciFirst = ciFirst item2 `mplus` ciFirst item1}
                Second -> item2 {ciSecond = ciSecond item2 `mplus` ciSecond item1}

            updateDepthMap m1 m2 = M.unionWith updateItem m1 m2

            item = case side of
                     First -> CacheItem {ciFirst = Just sideItem, ciSecond = Nothing}
                     Second -> CacheItem {ciFirst = Nothing, ciSecond = Just sideItem}

            init = M.singleton depth item

            newAicData = putBoardMapWith updateDepthMap board init (aicData aic)
            aic' = aic {aicDirty = True, aicData = newAicData}

            Just perBoard = lookupBoardMap board newAicData 

        writeTVar (aichData handle) aic'
        putWriteQueue (aichWriteQueue handle) (board, depth, side, sideItem)
        putCleanupQueue (aichCleanupQueue handle) (c, boardKey board) now


putAiCache :: AlphaBetaParams -> Board -> Int -> Side -> Score -> [Move] -> AICacheHandle rules -> Checkers ()
putAiCache params board depth side score moves handle = do
  let sideItem = CacheItemSide {cisScore = score}
  putAiCache' params board depth side sideItem handle
