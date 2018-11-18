{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.AlphaBeta.Cache where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Control.Monad.Metrics as Metrics
import qualified Data.Map as M
import qualified Data.HashPSQ as PQ
import Data.Maybe
import Data.Text.Format.Heavy (Single (..))
import System.FilePath
import System.Environment
import System.Directory
import System.Posix.IO
import qualified System.IO.RandomAccessFile as File
import System.Clock
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Parallel
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent

-- | Prepare AI storage instance.
-- This also contains Processor instance with several threads.
loadAiCache :: GameRules rules
            => (ScoreMoveInput rules -> Checkers (Move, Score))
            -> AlphaBeta rules
            -> Checkers (AICacheHandle rules)
loadAiCache scoreMove (AlphaBeta params rules) = do
  let getKey (ai, handle, side, depth, board, pm) = pmMove pm
  aiCfg <- asks (gcAiConfig . csConfig)
  processor <- runProcessor (aiThreads aiCfg) getKey scoreMove
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
  load <- asks (aiLoadCache . gcAiConfig . csConfig)
  store <- asks (aiStoreCache . gcAiConfig . csConfig)
  let mbMode
        | store = Just ReadWrite
        | load = Just ReadOnly
        | otherwise = Nothing
  (indexFile, dataFile, exist) <- case mbMode of
                    Nothing -> return (Nothing, Nothing, False)
                    Just mode -> do
                      indexExists <- liftIO $ doesFileExist indexPath
                      dataExists <- liftIO $ doesFileExist dataPath
                      let exist = dataExists && indexExists
                      if load && not store && not exist
                        then return (Nothing, Nothing, exist)
                        else liftIO $ do
                          let page = 1024*1024
                              params = (File.dfltCached $ File.MMapedParams page) {File.cachePageSize = page}
                          indexFd <- File.initFile params indexPath
                          dataFd <- File.initFile params dataPath
                          return (Just indexFd, Just dataFd, exist)
  when (isJust mbMode) $
      $info "Opened cache: {}" (Single cachePath)

  st <- ask
  indexLock <- liftIO RWL.new
  dataLock <- liftIO RWL.new
  indexBlockLocks <- liftIO $ atomically $ newTVar M.empty
  dataBlockLocks <- liftIO $ atomically $ newTVar M.empty

  let indexHandle = case indexFile of
        Nothing -> Nothing
        Just fd -> Just $ FHandle {
                     fhOffset = 0,
                     fhHandle = fd
                   }
  let dataHandle = case dataFile of
        Nothing -> Nothing
        Just fd -> Just $ FHandle {
                     fhOffset = 0,
                     fhHandle = fd
                   }
  counts <- liftIO $ atomically $ newTVar $ BoardCounts 50 50 50 50
  let handle = AICacheHandle {
      aichRules = rules,
      aichData = cache,
      aichWriteQueue = writeQueue,
      aichCleanupQueue = cleanupQueue,
      aichCurrentCounts = counts,
      aichIndexFile = indexHandle,
      aichDataFile = dataHandle
    }
  when (store && isJust indexFile && not exist) $ do
     runStorage handle $ initFile
  when (store) $ do
    forkCheckers $ cacheDumper rules params handle
  forkCheckers $ cacheCleaner handle

  return handle

cacheDumper :: GameRules rules => rules -> AlphaBetaParams -> AICacheHandle rules -> Checkers ()
cacheDumper rules params handle = do
  store <- asks (aiStoreCache . gcAiConfig . csConfig)
  when (store) $ forever $ do
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
    delta <- liftIO $ atomically $ do
        aic <- readTVar (aichData handle)
        currentCounts <- readTVar (aichCurrentCounts handle)
        let cache = aicData aic
        let oldSize = boardMapSize cache
        let cache' = M.filterWithKey biggerCounts cache
            biggerCounts bc _ =
              (bcFirstMen bc + bcFirstKings bc + bcSecondMen bc + bcSecondKings bc) <=
              (bcFirstMen currentCounts + bcFirstKings currentCounts + bcSecondMen currentCounts + bcSecondKings currentCounts)
            aic' = aic {aicData = cache'}
            newSize = boardMapSize cache'
            delta = oldSize - newSize
        writeTVar (aichData handle) aic'
        return delta
    $info "cleanup: cleaned {} records" (Single delta)
      
    liftIO $ threadDelay $ 30 * 1000 * 1000

normalize :: BoardSize -> (BoardCounts,BoardKey,Side) -> (BoardCounts,BoardKey,Side)
normalize bsize (bc,bk,side) =
  let bk' = flipBoardKey bsize bk
      bc' = flipBoardCounts bc
  in  if bc' < bc
        then (bc', bk', opposite side)
        else (bc, bk, side)

-- | Look up for item in the cache. First lookup in the memory,
-- then in the file (if it is open).
lookupAiCache :: GameRules rules => AlphaBetaParams -> Board -> DepthParams -> Side -> AICacheHandle rules -> Checkers (Maybe CacheItemSide)
lookupAiCache params board depth side handle = do
    -- let bsize = boardSize (aichRules handle)
    -- let (bc, bk, side') = normalize bsize (boardCounts board, boardKey board, side)
    -- let fixSign = if side' == side then id else negate
    let bc = boardCounts board
        bk = boardKey board
    cached <- lookupMemory (bc, bk) side
    case cached of
      Just result -> do
        Metrics.increment "cache.hit.memory"
        queueCleanup bc bk
        return cached
--         return $ Just $ CacheItemSide $ fixSign $ cisScore result
      Nothing -> do
        mbValue <- runStorage handle $ event "file lookup" $ lookupFile board depth side
        case mbValue of
          Nothing -> do
            Metrics.increment "cache.miss"
            return Nothing
          Just value -> do
            Metrics.increment "cache.hit.file"
            putAiCache' params board depth side value handle
            return mbValue

  where 
    queueCleanup bc bk = return ()
--     queueCleanup bc bk = do
--       let key = (bc, bk)
--       now <- liftIO $ getTime Monotonic
--       liftIO $ atomically $ putCleanupQueue (aichCleanupQueue handle) key now
    
    lookupMemory :: (BoardCounts, BoardKey) -> Side -> Checkers (Maybe CacheItemSide)
    lookupMemory (bc, bk) side = Metrics.timed "cache.lookup.memory" $ do
      let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
      cfg <- asks (gcAiConfig . csConfig)
      if total <= aiUseCacheMaxPieces cfg && dpTarget depth >= aiUseCacheMaxDepth cfg
        then do
            AICache _ _ cache <- liftIO $ atomically $ readTVar (aichData handle)
            case lookupBoardMap (bc,bk) cache of
              Nothing -> return Nothing
              Just byDepth -> do
                let ds = [dpTarget depth .. dpTarget depth + aiUseCacheMaxDepthPlus cfg] ++
                             [dpTarget depth - aiUseCacheMaxDepthMinus cfg .. dpTarget depth-1] 
                    depths = [depth {dpTarget = d} | d <- ds]
                case foldl mplus Nothing [M.lookup d byDepth | d <- depths ] of
                  Nothing -> return Nothing
                  Just item -> case side of
                                 First -> return $ ciFirst item
                                 Second -> return $ ciSecond item
        else return Nothing

-- | Put an item to the cache.
-- It is always writen to the memory,
-- and it is writen to the file if it is open.
putAiCache' :: GameRules rules => AlphaBetaParams -> Board -> DepthParams -> Side -> StorageValue -> AICacheHandle rules -> Checkers ()
putAiCache' params board depth side sideItem handle = do
  let bc = boardCounts board
      bk = boardKey board
  let bsize = boardSize (aichRules handle)
  --let (bc', bk', side') = normalize bsize (bc, bk, side)
  let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
  cfg <- asks (gcAiConfig . csConfig)
  when (total <= aiUpdateCacheMaxPieces cfg && dpTarget depth > aiUpdateCacheMaxDepth cfg) $ Metrics.timed "cache.put.memory" $ do
      now <- liftIO $ getTime Monotonic
      Metrics.increment "cache.records.put"
      store <- asks (aiStoreCache . gcAiConfig . csConfig)
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

            newAicData = putBoardMapWith updateDepthMap (bc,bk) init (aicData aic)
            aic' = aic {aicDirty = True, aicData = newAicData}

            Just perBoard = lookupBoardMap (bc,bk) newAicData 

        writeTVar (aichData handle) aic'
        when (store) $
            putWriteQueue (aichWriteQueue handle) (board, depth, side, sideItem)
        putCleanupQueue (aichCleanupQueue handle) (bc, bk) now


putAiCache :: GameRules rules => AlphaBetaParams -> Board -> DepthParams -> Side -> Score -> [Move] -> AICacheHandle rules -> Checkers ()
putAiCache params board depth side score moves handle = do
  let sideItem = CacheItemSide {cisScore = score}
  putAiCache' params board depth side sideItem handle

