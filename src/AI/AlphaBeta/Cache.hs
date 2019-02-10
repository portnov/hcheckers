{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module AI.AlphaBeta.Cache
  ( loadAiCache,
    lookupAiCache,
    putAiCache,
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException)
import qualified Control.Concurrent.ReadWriteLock as RWL
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
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent

-- | Prepare AI storage instance.
-- This also contains Processor instance with several threads.
loadAiCache :: (GameRules rules, Evaluator eval)
            => (ScoreMoveInput rules eval -> Checkers (PossibleMove, Score))
            -> AlphaBeta rules eval
            -> Checkers (AICacheHandle rules eval)
loadAiCache scoreMove (AlphaBeta params rules eval) = do
  let getKey input = pmResult (smiMove input)
  aiCfg <- asks (gcAiConfig . csConfig)
  processor <- runProcessor (aiThreads aiCfg) getKey scoreMove
  cache <- liftIO newTBoardMap
  cachePath <- do
              home <- liftIO $ getEnv "HOME"
              let directory = home </> ".cache" </> "hcheckers" </> rulesName rules </> "ai.cache"
              liftIO $ createDirectoryIfMissing True directory
              return directory
  let indexPath = cachePath </> "index"
      dataPath = cachePath </> "data"

  writeQueue <- liftIO $ atomically newTChan
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
                              params = File.MMapedParams page True
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
  moves <- liftIO newTBoardMap
  let handle = AICacheHandle {
      aichRules = rules,
      aichData = cache,
      aichProcessor = processor,
      aichPossibleMoves = moves,
      aichWriteQueue = writeQueue,
      aichCleanupQueue = cleanupQueue,
      aichCurrentCounts = counts,
      aichIndexFile = indexHandle,
      aichDataFile = dataHandle
    }
  when (store && isJust indexFile && not exist) $ do
     runStorage handle initFile
  when store $
    forkCheckers $ cacheDumper rules params handle
  -- forkCheckers $ cacheCleaner handle

  return handle

cacheDumper :: (GameRules rules, Evaluator eval) => rules -> AlphaBetaParams -> AICacheHandle rules eval -> Checkers ()
cacheDumper rules params handle = do
  store <- asks (aiStoreCache . gcAiConfig . csConfig)
  when store $ forever $ do
    repeatTimed "write" 30 $ do
      -- threadDelay $ 100*1000
      mbRecord <- liftIO $ atomically $ checkWriteQueue (aichWriteQueue handle)
      case mbRecord of
        Nothing -> return False
        Just (board, value) -> do
          Monitoring.increment "cache.records.writen"
          runStorage handle $
              putRecordFile board value
          return True

    liftIO $ threadDelay $ 30 * 1000 * 1000

cacheCleaner :: AICacheHandle rules eval -> Checkers ()
cacheCleaner handle = forever $ do
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
lookupAiCache :: (GameRules rules, Evaluator eval) => AlphaBetaParams -> Board -> DepthParams -> AICacheHandle rules eval -> Checkers (Maybe PerBoardData)
lookupAiCache params board depth handle = do
    mbItem <- lookupMemory board
    case mbItem of
      Just item -> do
        Monitoring.increment "cache.hit.memory"
        return $ Just item
      Nothing -> do
        mbFile <- lookupFile' board depth
        case mbFile of
          Nothing -> do
            Monitoring.increment "cache.miss"
            return Nothing
          Just file -> do
            let mbStats = checkStats =<< boardStats file
            let file' = file {boardStats = mbStats}
            putAiCache params board file' handle
            return $ Just file'

  where

    avg :: Stats -> Score
    avg s =
      let Score n p = statsSumScore s
          cnt = statsCount s
      in  Score (n `div` cnt) (p `div` cnt)

    checkStats :: Stats -> Maybe Stats
    checkStats s
      | statsCount s < 10 = Nothing
      | otherwise = Just s

    lookupMemory :: Board -> Checkers (Maybe PerBoardData)
    lookupMemory board = Monitoring.timed "cache.lookup.memory" $ do
      cfg <- asks (gcAiConfig . csConfig)
      let cache = aichData handle
      mbItem <- liftIO $ lookupBoardMap cache board 
      case mbItem of
        Nothing -> return Nothing
        Just item@(PerBoardData {..}) ->
          if itemDepth >= dpLast depth
            then return $ Just item
            else return Nothing

    lookupFile' :: Board -> DepthParams -> Checkers (Maybe PerBoardData)
    lookupFile' board depth = do
      let bc = calcBoardCounts board
      let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
      cfg <- asks (gcAiConfig . csConfig)
      if {-total <= aiUseCacheMaxPieces cfg &&-} dpTarget depth >= aiUseCacheMaxDepth cfg
        then runStorage handle (lookupFile board depth)
              `catch`
                  \(e :: SomeException) -> do
                      $reportError "Exception: lookupFile: {}" (Single $ show e)
                      return Nothing
        else return Nothing

-- | Put an item to the cache.
-- It is always writen to the memory,
-- and it is writen to the file if it is open.
putAiCache :: GameRules rules => AlphaBetaParams -> Board -> StorageValue -> AICacheHandle rules eval -> Checkers ()
putAiCache params board newItem handle = do
  let bc = calcBoardCounts board
  let bsize = boardSize (aichRules handle)
  let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
  let depth = itemDepth newItem
  cfg <- asks (gcAiConfig . csConfig)
  let needWriteFile = {-total <= aiUpdateCacheMaxPieces cfg &&-} depth > aiUpdateCacheMaxDepth cfg
  Monitoring.timed "cache.put.memory" $ do
    now <- liftIO $ getTime Monotonic
    Monitoring.increment "cache.records.put"
    fileCacheEnabled <- asks (aiStoreCache . gcAiConfig . csConfig)
    let cache = aichData handle
    liftIO $ putBoardMapWith cache (<>) board newItem

    liftIO $ atomically $ do
      when (fileCacheEnabled && needWriteFile) $
          putWriteQueue (aichWriteQueue handle) (board, newItem)
      -- putCleanupQueue (aichCleanupQueue handle) (bc, bk) now

