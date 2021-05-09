{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module AI.AlphaBeta.Cache
  ( loadAiCache,
    allocateCache,
    lookupAiCache, lookupAiCacheS,
    putAiCache, putAiCacheS,
    resetAiCache,
    mkCacheKey
  ) where

import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified StmContainers.Map as SM
import qualified Data.Map as M
import Data.Text.Format.Heavy (Single (..))
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import qualified Core.AdaptiveMap as AM
import Core.Board
import Core.BoardMap
import Core.Parallel
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent

mkCacheKey :: VectorEvaluator eval => eval -> Board -> CacheKey
mkCacheKey eval board = (evalToVector eval, board)

putAiData :: AIData -> CacheKey -> PerBoardData -> Checkers ()
putAiData aiData (evalVec, board) item =
  liftIO $ atomically $ do
    perEval <- readTVar aiData
    forEval <- case AM.lookup evalVec perEval of
                 Just map -> return map
                 Nothing -> do
                    map <- SM.new
                    writeTVar aiData $ AM.insert evalVec map perEval
                    return map
    putBoardMapWith' forEval (<>) board item

lookupAiData :: AIData -> CacheKey -> Checkers (Maybe PerBoardData)
lookupAiData aiData (evalVec, board) =
  liftIO $ atomically $ do
    perEval <- readTVar aiData
    case AM.lookup evalVec perEval of
      Nothing -> return Nothing
      Just forEval -> lookupBoardMap' forEval board

allocateCache :: VectorEvaluator eval => AICacheHandle rules eval -> eval -> Checkers BoardDataCache
allocateCache handle eval = do
  let aiData = aichData handle
  let evalVec = evalToVector eval
  liftIO $ atomically $ do
    perEval <- readTVar aiData
    case AM.lookup evalVec perEval of
      Nothing -> do
        newCache <- SM.new
        let perEval' = AM.insert evalVec newCache perEval
        writeTVar aiData perEval'
        return newCache
      Just cache -> return cache

-- | Prepare AI storage instance.
-- This also contains Processor instance with several threads.
loadAiCache :: (GameRules rules, Evaluator eval)
            => ([ScoreMoveInput rules eval] -> Checkers [MoveAndScore])
            -> AlphaBeta rules eval
            -> Checkers (AICacheHandle rules eval)
loadAiCache scoreMove (AlphaBeta params rules eval) = do
  let getKey inputs = map smiIndex inputs
  aiCfg <- asks (gcAiConfig . csConfig)
  processor <- runProcessor (aiThreads aiCfg) getKey scoreMove
  cache <- loadAiData rules

  st <- ask
  counts <- liftIO $ atomically $ newTVar $ BoardCounts 50 50 50 50
  moves <- liftIO newTBoardMap
  scoreShift <- liftIO $ atomically $ newTVar M.empty
  index <- liftIO $ atomically $ newTVar 0
  let handle = AICacheHandle {
      aichRules = rules,
      aichData = cache,
      aichJobIndex = index,
      aichProcessor = processor,
      aichPossibleMoves = moves,
      aichLastMoveScoreShift = scoreShift,
      aichCurrentCounts = counts
    }

  save <- asks (aiStoreCache . gcAiConfig . csConfig)
  when save $
      void $ forkCheckers $ aiStorageSaver rules cache

  return handle

aiStorageSaver :: GameRules rules => rules -> AIData -> Checkers ()
aiStorageSaver rules aiData = do
      saveAiData rules aiData
      liftIO $ threadDelay $ 10 * 1000 * 1000
      aiStorageSaver rules aiData

normalize :: BoardSize -> (BoardCounts,BoardKey,Side) -> (BoardCounts,BoardKey,Side)
normalize bsize (bc,bk,side) =
  let bk' = flipBoardKey bsize bk
      bc' = flipBoardCounts bc
  in  if bc' < bc
        then (bc', bk', opposite side)
        else (bc, bk, side)

-- | Look up for item in the cache. First lookup in the memory,
-- then in the file (if it is open).
lookupAiCache :: (GameRules rules) => CacheKey -> DepthParams -> AICacheHandle rules eval -> Checkers (Maybe PerBoardData)
lookupAiCache key depth handle = do
    mbItem <- lookupMemory
    case mbItem of
      Just item -> do
        Monitoring.increment "cache.hit.memory"
        return $ Just item
      Nothing -> do
        Monitoring.increment "cache.miss"
        return Nothing
  where

    lookupMemory :: Checkers (Maybe PerBoardData)
    lookupMemory = Monitoring.timed "cache.lookup.memory" $ do
      -- cfg <- asks (gcAiConfig . csConfig)
      let cache = aichData handle
      mbItem <- lookupAiData cache key
      case mbItem of
        Nothing -> return Nothing
        Just item@(PerBoardData {..}) ->
          if itemDepth >= dpLast depth
            then return $ Just item
            else return Nothing

-- | Look up for item in the cache.
lookupAiCacheS :: CacheKey -> DepthParams -> BoardDataCache -> ScoreM rules eval (Maybe PerBoardData)
lookupAiCacheS key@(_,board) depth cache = lift $ do
    mbItem <- lookupMemory
    case mbItem of
      Just item -> do
        Monitoring.increment "cache.hit.memory"
        return $ Just item
      Nothing -> do
        Monitoring.increment "cache.miss"
        return Nothing
  where
    lookupMemory :: Checkers (Maybe PerBoardData)
    lookupMemory = Monitoring.timed "cache.lookup.memory" $ do
      mbItem <- liftIO $ atomically $ lookupBoardMap' cache board
      case mbItem of
        Nothing -> return Nothing
        Just item@(PerBoardData {..}) ->
          if itemDepth >= dpLast depth
            then return $ Just item
            else return Nothing

-- | Put an item to the cache.
putAiCache :: (GameRules rules) => CacheKey -> CacheValue -> AICacheHandle rules eval -> Checkers ()
putAiCache key newItem handle = do
  Monitoring.timed "cache.put.memory" $ do
    Monitoring.increment "cache.records.put"
    let cache = aichData handle
    putAiData cache key newItem

-- | Put an item to the cache.
putAiCacheS :: CacheKey -> CacheValue -> BoardDataCache -> ScoreM rules eval ()
putAiCacheS key@(_,board) newItem cache = lift $ do
  Monitoring.timed "cache.put.memory" $ do
    Monitoring.increment "cache.records.put"
    liftIO $ atomically $ putBoardMapWith' cache (<>) board newItem

resetAiCache :: AICacheHandle rules eval -> Checkers ()
resetAiCache handle = do
  let cache = aichData handle
  liftIO $ atomically $
    writeTVar cache $ AM.empty 2

