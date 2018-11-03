{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module AI.AlphaBeta where

import Control.Monad
import Control.Monad.State
import qualified Control.Monad.Metrics as Metrics
import Control.Concurrent.STM
import Data.Maybe
import Data.Text.Format.Heavy
import Data.Aeson
import Text.Printf
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.Evaluator
import Core.BoardMap
import Core.Parallel
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"
      <*> v .:? "start_depth"
      <*> v .:? "max_combination_depth" .!= 8
      <*> v .:? "threads" .!= 4
      <*> v .:? "load" .!= True
      <*> v .:? "store" .!= False
      <*> v .:? "use_cache_max_depth" .!= 8
      <*> v .:? "use_cache_max_pieces" .!= 24
      <*> v .:? "use_cache_max_depth_plus" .!= 2
      <*> v .:? "use_cache_max_depth_minus" .!= 0
      <*> v .:? "update_cache_max_depth" .!= 6
      <*> v .:? "update_cache_max_pieces" .!= 8

instance (GameRules rules) => GameAi (AlphaBeta rules) where

  type AiStorage (AlphaBeta rules) = AICacheHandle rules

  createAiStorage ai = do
    cache <- loadAiCache scoreMove ai
    return cache

  saveAiStorage (AlphaBeta params rules) cache = do
      -- saveAiCache rules params cache
      return ()

  chooseMove ai storage side board = do
    (moves, _) <- runAI ai storage side board
    liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ boardCounts board
    return moves

  updateAi ai@(AlphaBeta _ rules) json =
    case fromJSON json of
      Error _ -> ai
      Success params -> AlphaBeta params rules

  aiName _ = "default"

scoreMove :: (GameRules rules) => ScoreMoveInput rules -> Checkers (Move, Score)
scoreMove (ai@(AlphaBeta params rules), var, side, dp, board, pm) = do
     score <- Metrics.timed "ai.score.move" $ do
                let board' = applyMoveActions (pmResult pm) board
                score <- doScore rules ai var params (opposite side) dp board'
                $info "Check: {} (depth {}) => {}" (show pm, dpTarget dp, score)
                return score
     
     return (pmMove pm, score)

runAI :: GameRules rules => AlphaBeta rules -> AICacheHandle rules -> Side -> Board -> Checkers ([Move], Score)
runAI ai@(AlphaBeta params rules) handle side board = do
    let depth = abDepth params
    let moves = possibleMoves rules side board
    let var = aichData handle
    AICache _ processor _ <- liftIO $ atomically $ readTVar var
    dp <- updateDepth (length moves) $ DepthParams {
               dpTarget = depth
             , dpCurrent = -1
             , dpMax = abCombinationDepth params + depth
             , dpMin = fromMaybe depth (abStartDepth params)
             }
    let inputs = [(ai, handle, side, dp, board, move) | move <- moves]
    scores <- process processor inputs
    let select = if side == First then maximum else minimum
        maxScore = select $ map snd scores
        goodMoves = [move | (move, score) <- scores, score == maxScore]
    return (goodMoves, maxScore)

-- type ScoreMemo = M.Map Side (M.Map Int (M.Map Score (M.Map Score Score)))

data MovesMemo = MovesMemo {
    mmFirst :: BoardMap [PossibleMove],
    mmSecond :: BoardMap [PossibleMove]
  }

lookupMoves :: Side -> Board -> MovesMemo -> Maybe [PossibleMove]
lookupMoves First board memo = lookupBoardMap (boardCounts board, boardKey board) (mmFirst memo)
lookupMoves Second board memo = lookupBoardMap (boardCounts board, boardKey board) (mmSecond memo)

putMoves :: Side -> Board -> [PossibleMove] -> MovesMemo -> MovesMemo
putMoves First board moves memo =
  memo {mmFirst = putBoardMap board moves (mmFirst memo)}
putMoves Second board moves memo =
  memo {mmSecond = putBoardMap board moves (mmSecond memo)}

doScore :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules -> AlphaBetaParams -> Side -> DepthParams -> Board -> Checkers Score
doScore rules eval var params side dp board =
    fixSign <$> evalStateT (cachedScoreAB var params side dp (-max_score) max_score board) initState
  where
    initState = ScoreState rules eval [StackItem Nothing (-max_score)] emptyMemo
    emptyMemo = MovesMemo emptyBoardMap emptyBoardMap

    fixSign s = s
--       | side == First = s
--       | otherwise = negate s

data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssStack :: ! [StackItem]
  , ssMemo :: ! MovesMemo
  }

data StackItem = StackItem {
    siMove :: Maybe PossibleMove
  , siScoreBest :: ! Score
  }

instance Show StackItem where
  show si = printf "(%s)" (showM (siMove si))
    where
      showM Nothing = "Start"
      showM (Just move) = show move

type ScoreM rules eval a = StateT (ScoreState rules eval) Checkers a

instance HasLogger (StateT (ScoreState rules eval) Checkers) where
  getLogger = lift getLogger

  localLogger logger actions = do
    st <- get
    (result, st') <- lift $ localLogger logger $ runStateT actions st
    put st'
    return result

instance HasLogContext (StateT (ScoreState rules eval) Checkers) where
  getLogContext = lift getLogContext

  withLogContext frame actions = do
    st <- get
    (result, st') <- lift $ withLogContext frame $ runStateT actions st
    put st'
    return result

cachedScoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
              => AICacheHandle rules
              -> AlphaBetaParams
              -> Side
              -> DepthParams
              -> Score
              -> Score
              -> Board
              -> ScoreM rules eval Score
cachedScoreAB var params side dp alpha beta board = do
  let depth = dpCurrent dp
  mbItem <- lift $ lookupAiCache params board depth side var
  case mbItem of
    Just item -> do
      -- lift $ putStrLn "Cache hit"
      let score = cisScore item
      if score < alpha
        then return alpha
        else if score > beta
               then return beta
               else return score
    Nothing -> do
      (score, moves) <- Metrics.timed "ai.score.board" $ scoreAB var params side dp alpha beta board
      lift $ putAiCache params board depth side score moves var
      return score

isTargetDepth :: DepthParams -> Bool
isTargetDepth dp = dpCurrent dp >= dpTarget dp

updateDepth :: (Monad m, HasLogging m, MonadIO m) => Int -> DepthParams -> m DepthParams
updateDepth nMoves dp
  | nMoves == 1 = do
                let target = min (dpTarget dp + 1) (dpMax dp)
                let indent = replicate (2*dpCurrent dp) ' '
                $debug "{}| there is only one move, increase target depth to {}"
                        (indent, target)
                return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target}
  | nMoves > 8 = do
                let target = max (dpCurrent dp + 1) (dpMin dp)
                let indent = replicate (2*dpCurrent dp) ' '
                $debug "{}| there are too many moves, decrease target depth to {}"
                        (indent, target)
                return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target}
  | otherwise = return $ dp {dpCurrent = dpCurrent dp + 1}

scoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
        => AICacheHandle rules
        -> AlphaBetaParams
        -> Side
        -> DepthParams
        -> Score
        -> Score
        -> Board
        -> ScoreM rules eval (Score, [Move])
scoreAB var params side dp alpha beta board
  | isTargetDepth dp = do
      evaluator <- gets ssEvaluator
      let score0 = evalBoard evaluator First side board
      $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, alpha, beta, score0)
      return (score0, [])
  | otherwise = do
      rules <- gets ssRules
      setBest $ if maximize then alpha else beta -- we assume alpha <= beta
      $trace "{}V Side: {}, A = {}, B = {}" (indent, show side, alpha, beta)
      moves <- possibleMoves' board

      when (null moves) $
        $trace "{}| No moves left." (Single indent)

      dp' <- updateDepth (length moves) dp
      iterateMoves moves dp'

  where

    possibleMoves' :: Board -> ScoreM rules eval [PossibleMove]
    possibleMoves' board = do
--         rules <- gets ssRules
--         return $ possibleMoves rules side board
      rules <- gets ssRules
      memo <- gets ssMemo
      case lookupMoves side board memo of
        Just moves -> return moves
        Nothing -> do
          let moves = possibleMoves rules side board
          modify $ \st -> st {ssMemo = putMoves side board moves (ssMemo st)}
          return moves

    maximize = side == First
    minimize = not maximize

    bestStr :: String
    bestStr = if maximize
                then "Maximum"
                else "Minimum"
    
    push :: PossibleMove -> ScoreM rules eval ()
    push move =
      modify $ \st -> st {ssStack = (StackItem (Just move) (-max_score)) : (ssStack st)}

    pop = 
      modify $ \st -> st {ssStack = tail (ssStack st)}

    showStack s = unwords $ map show s

    printStack = do
      stack <- gets (reverse . ssStack)
      $trace "{}| side {}: {}" (indent, show side, showStack stack)

    indent = replicate (2*dpCurrent dp) ' '

    getBest =
      gets (siScoreBest . head . ssStack)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      $trace "{}| {} for depth {} : {} => {}" (indent, bestStr, dpCurrent dp, oldBest, best)
      modify $ \st -> st {ssStack = update (head $ ssStack st) : tail (ssStack st)}
        where
          update item = item {siScoreBest = best}

    iterateMoves :: [PossibleMove] -> DepthParams -> ScoreM rules eval (Score, [Move])
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}|—All moves considered at this level, return best = {}" (indent, best)
      return (best, [])
    iterateMoves (move : moves) dp' = do
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      evaluator <- gets ssEvaluator
      rules <- gets ssRules
      let board' = applyMoveActions (pmResult move) board
      best <- getBest
      push move 
      printStack
      let alpha' = if maximize
                     then max alpha best
                     else alpha
          beta'  = if maximize
                     then beta
                     else min beta best
      score <- cachedScoreAB var params (opposite side) dp' alpha' beta' board'
      $trace "{}| score for side {}: {}" (indent, show side, score)
      pop
      best <- getBest

      -- trace (printf "%s| Score for depth %d = %d, prev.best = %d" indent depth score best) $ return ()
      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if score >= beta
               then do
                    best <- getBest
                    $trace "{}| Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, best)
                    return (best, [])
                    
               else iterateMoves moves dp'
        else do
             -- trace (printf "%s| Score for side %s = %d, go to next move." indent (show side) score) $ return ()
             iterateMoves moves dp'
        
instance Evaluator rules => Evaluator (AlphaBeta rules) where
  evaluatorName (AlphaBeta _ rules) = evaluatorName rules
  evalBoard (AlphaBeta _ rules) = evalBoard rules

