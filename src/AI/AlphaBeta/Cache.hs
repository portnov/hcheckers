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
    resetAiCache
  ) where

import Control.Monad.State
import Control.Monad.Reader
import Control.Concurrent.STM
import qualified StmContainers.Map as SM
import qualified Data.Map as M
import Data.Text.Format.Heavy (Single (..))
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Parallel
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types
-- import AI.AlphaBeta.Persistent

newAiData :: Checkers AIData
newAiData = liftIO $ atomically $ newTVar $ M.empty

putAiData :: VectorEvaluator eval => AIData -> eval -> Board -> PerBoardData -> Checkers ()
putAiData aiData eval board item =
  liftIO $ atomically $ do
    perEval <- readTVar aiData
    let vec = evalToVector eval
    forEval <- case M.lookup vec perEval of
                 Just map -> return map
                 Nothing -> do
                    map <- SM.new
                    writeTVar aiData $ M.insert vec map perEval
                    return map
    putBoardMapWith' forEval (<>) board item

lookupAiData :: VectorEvaluator eval => AIData -> eval -> Board -> Checkers (Maybe PerBoardData)
lookupAiData aiData eval board =
  liftIO $ atomically $ do
    perEval <- readTVar aiData
    let vec = evalToVector eval
    case M.lookup vec perEval of
      Nothing -> return Nothing
      Just forEval -> lookupBoardMap' forEval board

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
  cache <- newAiData

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

  return handle

normalize :: BoardSize -> (BoardCounts,BoardKey,Side) -> (BoardCounts,BoardKey,Side)
normalize bsize (bc,bk,side) =
  let bk' = flipBoardKey bsize bk
      bc' = flipBoardCounts bc
  in  if bc' < bc
        then (bc', bk', opposite side)
        else (bc, bk, side)

-- | Look up for item in the cache. First lookup in the memory,
-- then in the file (if it is open).
lookupAiCache :: (GameRules rules, VectorEvaluator eval) => eval -> AlphaBetaParams -> Board -> DepthParams -> AICacheHandle rules eval -> Checkers (Maybe PerBoardData)
lookupAiCache eval params board depth handle = do
    mbItem <- lookupMemory board
    case mbItem of
      Just item -> do
        Monitoring.increment "cache.hit.memory"
        return $ Just item
      Nothing -> do
        Monitoring.increment "cache.miss"
        return Nothing
  where

    lookupMemory :: Board -> Checkers (Maybe PerBoardData)
    lookupMemory board = Monitoring.timed "cache.lookup.memory" $ do
      cfg <- asks (gcAiConfig . csConfig)
      let cache = aichData handle
      mbItem <- lookupAiData cache eval board 
      case mbItem of
        Nothing -> return Nothing
        Just item@(PerBoardData {..}) ->
          if itemDepth >= dpLast depth
            then return $ Just item
            else return Nothing

-- | Put an item to the cache.
-- It is always writen to the memory,
-- and it is writen to the file if it is open.
putAiCache :: (GameRules rules, VectorEvaluator eval) => eval -> AlphaBetaParams -> Board -> StorageValue -> AICacheHandle rules eval -> Checkers ()
putAiCache eval params board newItem handle = do
  let bc = calcBoardCounts board
  let bsize = boardSize (aichRules handle)
  let total = bcFirstMen bc + bcSecondMen bc + bcFirstKings bc + bcSecondKings bc
  let depth = itemDepth newItem
  cfg <- asks (gcAiConfig . csConfig)
  Monitoring.timed "cache.put.memory" $ do
    Monitoring.increment "cache.records.put"
    let cache = aichData handle
    putAiData cache eval board newItem

resetAiCache :: AICacheHandle rules eval -> Checkers ()
resetAiCache handle = do
  let cache = aichData handle
  liftIO $ atomically $
    writeTVar cache M.empty

