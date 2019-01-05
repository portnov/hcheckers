{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

{-
 - This module contains an implementation of alpha-beta-pruning algorithm
 - with small improvements.
 -}

module AI.AlphaBeta where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Catch
import qualified Control.Monad.Metrics as Metrics
import Control.Concurrent.STM
import Data.Maybe
import Data.Int
import Data.List (sortOn)
import Data.Text.Format.Heavy
import Data.Aeson
import Text.Printf
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

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
      <*> v .:? "moves_bound_low" .!= 4
      <*> v .:? "moves_bound_high" .!= 8
      <*> v .:? "time"

instance (GameRules rules, Evaluator eval) => GameAi (AlphaBeta rules eval) where

  type AiStorage (AlphaBeta rules eval) = AICacheHandle rules eval

  createAiStorage ai = do
    cache <- loadAiCache scoreMove ai
    return cache

  saveAiStorage (AlphaBeta params rules _) cache = do
      -- saveAiCache rules params cache
      return ()

  chooseMove ai storage side board = do
    (moves, _) <- runAI ai storage side board
    liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ boardCounts board
    return moves

  updateAi ai@(AlphaBeta _ rules eval) json =
    case fromJSON json of
      Error _ -> ai
      Success params -> AlphaBeta params rules (updateEval eval json)

  aiName _ = "default"

scoreMove :: (GameRules rules, Evaluator eval) => ScoreMoveInput rules eval -> Checkers (PossibleMove, Score)
scoreMove (ai@(AlphaBeta params rules eval), var, side, dp, board, pm, alpha, beta) = do
     score <- Metrics.timed "ai.score.move" $ do
                let board' = applyMoveActions (pmResult pm) board
                score <- doScore rules eval var params (opposite side) dp board' alpha beta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show pm, dpTarget dp, show e)
                                        throwError e
                                  )
                $info "Check: {} (depth {}) => {}" (show pm, dpTarget dp, show score)
                return score
     
     return (pm, score)

type DepthIterationInput = (AlphaBetaParams, Maybe DepthIterationOutput)
type DepthIterationOutput = [(PossibleMove, Score)]
type AiOutput = ([PossibleMove], Score)

runAI :: (GameRules rules, Evaluator eval) => AlphaBeta rules eval -> AICacheHandle rules eval -> Side -> Board -> Checkers AiOutput
runAI ai@(AlphaBeta params rules eval) handle side board = do
    options <- depthDriver
    select options
  where
    maximize = side == First
    minimize = not maximize

    depthDriver :: Checkers DepthIterationOutput
    depthDriver = case abBaseTime params of
                 Nothing -> do
                    (result, _) <- go (params, Nothing)
                    return result
                 Just time -> repeatTimed' "runAI" time goTimed (params, Nothing)
  
    goTimed :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    goTimed (params, prevResult) = do
      ret <- tryC $ go (params, prevResult)
      case ret of
        Right result -> return result
        Left TimeExhaused ->
          case prevResult of
            Just result -> return (result, Nothing)
            Nothing -> do
              let moves = possibleMoves rules side board
              return ([(move, 0) | move <- moves], Nothing)
        Left err -> throwError err

    go :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    go (params, prevResult) = do
      let depth = abDepth params
      let moves = possibleMoves rules side board
      if length moves <= 1 -- Just one move possible
        then do
          $info "There is only one move possible; just do it." ()
          -- currently we do not use results of evaluating of all moves
          -- when evaluating deeper parts of the tree (it is hard due to alpha-beta restrictions).
          -- It means we are not going to use that Score value anyway.
          return ([(move, 0) | move <- moves], Nothing)
                                                             
        else do
          let var = aichData handle
          $info "Selecting a move. Side = {}, depth = {}, number of possible moves = {}" (show side, depth, length moves)
          dp <- updateDepth params (length moves) $ DepthParams {
                     dpTarget = depth
                   , dpCurrent = -1
                   , dpMax = abCombinationDepth params + depth
                   , dpMin = fromMaybe depth (abStartDepth params)
                   }
          result <- widthController True True prevResult moves dp initInterval
          -- In some corner cases, there might be 1 or 2 possible moves,
          -- so the timeout would allow us to calculate with very big depth;
          -- too big depth does not decide anything in such situations.
          if depth < 50
            then do
              let params' = params {abDepth = depth + 1, abStartDepth = Nothing}
              return (result, Just (params', Just result))
            else return (result, Nothing)

    -- | Initial (alpha, beta) interval
    initInterval :: (Score, Score)
    initInterval =
      let score0 = evalBoard eval First side board
          delta = 1
      in  (score0 - delta, score0 + delta)

    selectScale :: Score -> ScoreBase
    selectScale s
      | s > 10000 = 1000
      | s > 1000 = 10
      | s > 100 = 5
      | otherwise = 2

    nextInterval :: (Score, Score) -> (Score, Score)
    nextInterval (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scaleScore` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if maximize
            then (beta', max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), alpha')

    prevInterval :: (Score, Score) -> (Score, Score)
    prevInterval (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scaleScore` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if minimize
            then (beta', max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), alpha')

    widthController :: Bool -- ^ Allow to shift (alpha,beta) segment to bigger values?
                    -> Bool -- ^ Allow to shift (alpha,beta) segment to lesser values?
                    -> Maybe DepthIterationOutput -- ^ Results of previous depth iteration
                    -> [PossibleMove]
                    -> DepthParams
                    -> (Score, Score) -- ^ (Alpha, Beta)
                    -> Checkers DepthIterationOutput
    widthController allowNext allowPrev prevResult moves dp interval@(alpha,beta) = do
      if alpha == beta
        then do
          $info "Empty scores interval: [{}]. We have to think that all moves have this score." (Single alpha)
          return [(move, alpha) | move <- moves]
        else do
            results <- widthIteration prevResult moves dp interval
            let (good, badScore, badMoves) = selectBestEdge interval moves results
                (bestMoves, bestResults) = unzip good
            if length badMoves == length moves
              then
                if allowPrev
                  then do
                    let interval' = prevInterval interval
                    $info "All moves are `too bad'; consider worse scores interval: [{} - {}]" interval'
                    widthController False True prevResult badMoves dp interval'
                  else do
                    $info "All moves are `too bad' ({}), but we have already checked worse interval; so this is the real score." (Single badScore)
                    return [(move, badScore) | move <- moves]
              else
                case bestResults of
                  [] -> return results
                  [_] -> do
                    $info "Exactly one move is `too good'; do that move." ()
                    return bestResults
                  _ -> do
                    if allowNext
                      then do
                        let interval'@(alpha',beta') = nextInterval interval
                        $info "Some moves ({} of them) are `too good'; consider better scores interval: [{} - {}]" (length bestMoves, alpha', beta')
                        widthController True False prevResult bestMoves dp interval'
                      else do
                        $info  "Some moves ({} of them) are `too good'; but we have already checked better interval; so this is the real score" (Single $ length bestMoves)
                        return bestResults

    widthIteration :: Maybe DepthIterationOutput -> [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    widthIteration prevResult moves dp (alpha, beta) = do
      $info "`- Considering scores interval: [{} - {}]" (alpha, beta)
      let var = aichData handle
      AICache _ processor _ <- liftIO $ atomically $ readTVar var
      let inputs = [(ai, handle, side, dp, board, move, alpha, beta) | move <- moves]
      results <- process' processor inputs
      joined <- joinResults prevResult results
      return joined

    joinResults :: Maybe DepthIterationOutput -> [Either Error (PossibleMove, Score)] -> Checkers DepthIterationOutput
    joinResults Nothing results =
      case sequence results of
        Right result -> return result
        Left err -> throwError err
    joinResults (Just prevResults) results = zipWithM joinResult prevResults results

    joinResult :: (PossibleMove, Score) -> Either Error (PossibleMove, Score) -> Checkers (PossibleMove, Score)
    joinResult prev@(move, score) (Left TimeExhaused) = do
      $info "Time exhaused while checking move {}, use result from previous depth: {}" (show move, score)
      return prev
    joinResult _ (Left err) = throwError err
    joinResult _ (Right result) = return result

    selectBestEdge (alpha, beta) moves results =
      let (good, bad) = if maximize then (beta, alpha) else (alpha, beta)
          goodResults = [(move, (goodMoves, score)) | (move, (goodMoves, score)) <- zip moves results, score == good]
          badResults = [move | (move, (_, score)) <- zip moves results, score == bad]
      in  (goodResults, bad, badResults)

    select :: DepthIterationOutput -> Checkers AiOutput
    select pairs = do
      let best = if maximize then maximum else minimum
          maxScore = best $ map snd pairs
          goodMoves = [move | (move, score) <- pairs, score == maxScore]
      return (goodMoves, maxScore)

-- | Cache of possible moves per board
data MovesMemo = MovesMemo {
      mmFirst :: BoardMap [PossibleMove]
    , mmSecond :: BoardMap [PossibleMove]
  }

lookupMoves :: Side -> Board -> MovesMemo -> Maybe [PossibleMove]
lookupMoves First board memo = lookupBoardMap (boardCounts board, boardKey board) (mmFirst memo)
lookupMoves Second board memo = lookupBoardMap (boardCounts board, boardKey board) (mmSecond memo)

putMoves :: Side -> Board -> [PossibleMove] -> MovesMemo -> MovesMemo
putMoves First board moves memo =
  memo {mmFirst = putBoardMap board moves (mmFirst memo)}
putMoves Second board moves memo =
  memo {mmSecond = putBoardMap board moves (mmSecond memo)}

-- | Calculate score of the board
doScore :: (GameRules rules, Evaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> Side
        -> DepthParams
        -> Board
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers Score
doScore rules eval var params side dp board alpha beta =
    evalStateT (cachedScoreAB var params input) =<< initState
  where
    input = ScoreInput side dp alpha beta board Nothing 
    initState = do
      now <- liftIO $ getTime Monotonic
      let timeout = case abBaseTime params of
                      Nothing -> Nothing
                      Just sec -> Just $ TimeSpec (fromIntegral sec) 0
      return $ ScoreState rules eval [loose] emptyMemo now timeout
    emptyMemo = MovesMemo emptyBoardMap emptyBoardMap

data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssBestScores :: [Score] -- ^ At each level of depth-first search, there is own "best score"
  , ssMemo :: ! MovesMemo   -- ^ Cache of possible moves
  , ssStartTime :: TimeSpec -- ^ Start time of calculation
  , ssTimeout :: Maybe TimeSpec -- ^ Nothing for "no timeout"
  }

data ScoreInput = ScoreInput {
    siSide :: Side
  , siDepth :: DepthParams
  , siAlpha :: Score
  , siBeta :: Score
  , siBoard :: Board
  , siPrevMove :: Maybe PossibleMove
  }

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

-- | Calculate score of the board. 
-- This uses the cache. It is called in the recursive call also.
cachedScoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
              => AICacheHandle rules eval
              -> AlphaBetaParams
              -> ScoreInput
              -> ScoreM rules eval Score
cachedScoreAB var params input = do
  let depth = dpCurrent dp
      side = siSide input
      board = siBoard input
      dp = siDepth input
      alpha = siAlpha input
      beta = siBeta input
  mbItem <- lift $ lookupAiCache params board dp side var
  case mbItem of
    Just item -> do
      $trace "Cache hit" ()
      let score = cisScore item
      -- it is possible that this value was put to cache with different
      -- values of alpha/beta; but we have to maintain the property of
      -- AB-section: alpha <= result <= beta. So here we clamp the value
      -- that we got from cache.
      if score < alpha
        then return alpha
        else if score > beta
               then return beta
               else return score

    Nothing -> do
      score <- Metrics.timed "ai.score.board" $ scoreAB var params input
      when (alpha < score && score < beta) $
          -- we can only put the result to the cache if we know
          -- that this score was not clamped by alpha or beta
          -- (so this is a real score, not alpha/beta bound)
          lift $ putAiCache params board dp side score [] var
      return score

-- | Check if target depth is reached
isTargetDepth :: DepthParams -> Bool
isTargetDepth dp = dpCurrent dp >= dpTarget dp

-- | Increase current depth as necessary.
--
-- If there is only 1 move currently possible, this can increase
-- the target depth, up to dpMax. Such situations mean that there is
-- probably a series of captures going on, which can change situation
-- dramatically. So we want to know the result better (up to the end of
-- the whole combination, if possible) to make our choice.
--
-- If there are a lot of moves possible, this can decrease the
-- target depth, down to dpMin. This is done simply to decrease computation
-- time. This is obviously going to lead to less strong play.
--
-- Otherwise, this just increases dpCurrent by 1.
--
updateDepth :: (Monad m, HasLogging m, MonadIO m) => AlphaBetaParams -> Int -> DepthParams -> m DepthParams
updateDepth params nMoves dp
  | nMoves <= abMovesLowBound params = do
                let delta = nMoves - 1
                let target = min (dpTarget dp + 1) (dpMax dp - delta)
                let indent = replicate (2*dpCurrent dp) ' '
                $debug "{}| there is only one move, increase target depth to {}"
                        (indent, target)
                return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target}
  | nMoves > abMovesHighBound params = do
                let target = max (dpCurrent dp + 1) (dpMin dp)
                let indent = replicate (2*dpCurrent dp) ' '
                $debug "{}| there are too many moves, decrease target depth to {}"
                        (indent, target)
                return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target}
  | otherwise = return $ dp {dpCurrent = dpCurrent dp + 1}

isTimeExhaused :: ScoreM rules eval Bool
isTimeExhaused = do
  check <- gets ssTimeout
  case check of
    Nothing -> return False
    Just delta -> do
      start <- gets ssStartTime
      now <- liftIO $ getTime Monotonic
      return $ start + delta <= now

-- | Calculate score for the board
scoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
        => AICacheHandle rules eval
        -> AlphaBetaParams
        -> ScoreInput
        -> ScoreM rules eval Score
scoreAB var params input
  | isTargetDepth dp = do
      -- target depth is achieved, calculate score of current board directly
      evaluator <- gets ssEvaluator
      let score0 = evalBoard evaluator First side board
      $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      return score0
  | otherwise = do
      -- first, let "best" be the worse possible value
      let best = if maximize then alpha else beta -- we assume alpha <= beta
      push best
      $trace "{}V Side: {}, A = {}, B = {}" (indent, show side, show alpha, show beta)
      rules <- gets ssRules
      let moves = possibleMoves rules side board
      -- moves <- possibleMoves' board

      -- this actually means that corresponding side lost.
      when (null moves) $
        $trace "{}`—No moves left." (Single indent)

      dp' <- updateDepth params (length moves) dp
      let prevMove = siPrevMove input
      score <- iterateMoves moves dp'
      pop
      return score

  where

    side = siSide input
    dp = siDepth input
    alpha = siAlpha input
    beta = siBeta input
    board = siBoard input

    push :: Score -> ScoreM rules eval ()
    push score =
      modify $ \st -> st {ssBestScores = score : ssBestScores st}

    pop :: ScoreM rules eval ()
    pop =
      modify $ \st -> st {ssBestScores = tail (ssBestScores st)}

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

    sortMoves :: Maybe PossibleMove -> [PossibleMove] -> [PossibleMove]
    sortMoves Nothing moves = moves
    sortMoves (Just prev) moves = sortOn (distance prev) moves

    distance :: PossibleMove -> PossibleMove -> Line
    distance prev pm =
      let Label col row = aLabel (pmEnd prev)
          Label col' row' = aLabel (pmBegin pm)
      in  abs (col' - col) `max` abs (row' - row)

    maximize = side == First
    minimize = not maximize

    bestStr :: String
    bestStr = if maximize
                then "Maximum"
                else "Minimum"
    
    indent = replicate (2*dpCurrent dp) ' '

    getBest =
      gets (head . ssBestScores)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      $trace "{}| {} for depth {} : {} => {}" (indent, bestStr, dpCurrent dp, show oldBest, show best)
      modify $ \st -> st {ssBestScores = best : tail (ssBestScores st)}

    iterateMoves :: [PossibleMove] -> DepthParams -> ScoreM rules eval Score
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}`—All moves considered at this level, return best = {}" (indent, show best)
      return best
    iterateMoves (move : moves) dp' = do
      timeout <- isTimeExhaused
      when timeout $ do
        -- $info "Timeout exhaused for depth {}." (Single $ dpCurrent dp)
        throwError TimeExhaused
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      evaluator <- gets ssEvaluator
      rules <- gets ssRules
      best <- getBest
      let input' = input {
                      siSide = opposite side
                    , siAlpha = if maximize
                                 then max alpha best
                                 else alpha
                    , siBeta = if maximize
                                 then beta
                                 else min beta best
                    , siPrevMove = Just move
                    , siBoard = applyMoveActions (pmResult move) board
                    , siDepth = dp'
                  }
      score <- cachedScoreAB var params input'
      $trace "{}| score for side {}: {}" (indent, show side, show score)

      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if (maximize && score >= beta) || (minimize && score <= alpha)
               then do
                    $trace "{}`—Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, show score)
                    return score
                    
               else iterateMoves moves dp'
        else do
             iterateMoves moves dp'
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks whoMovesNext board =
    evalBoard eval whoAsks whoMovesNext board

