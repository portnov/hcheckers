{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

{-
 - This module contains an implementation of alpha-beta-pruning algorithm
 - with small improvements.
 -}

module AI.AlphaBeta
  ( runAI, scoreMove
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Maybe
import Data.List (sortOn)
import Data.Text.Format.Heavy
import Data.Aeson
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Parallel
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"
      <*> v .:? "start_depth"
      <*> v .:? "max_combination_depth" .!= 8
      <*> v .:? "deeper_if_bad" .!= False
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

  chooseMove ai storage gameId side board = do
    (moves, _) <- runAI ai storage gameId side board
    -- liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ calcBoardCounts board
    return moves

  updateAi ai@(AlphaBeta _ rules eval) json =
    case fromJSON json of
      Error _ -> ai
      Success params -> AlphaBeta params rules (updateEval eval json)

  aiName _ = "default"

-- | Calculate score of one possible move.
scoreMove :: (GameRules rules, Evaluator eval) => ScoreMoveInput rules eval -> Checkers (PossibleMove, Score)
scoreMove (ScoreMoveInput {..}) = do
     let AlphaBeta params rules eval = smiAi
     score <- Monitoring.timed "ai.score.move" $ do
                let board' = applyMoveActions (pmResult smiMove) smiBoard
                score <- doScore rules eval smiCache params smiGameId (opposite smiSide) smiDepth board' smiGlobalInterval smiAlpha smiBeta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show smiMove, dpTarget smiDepth, show e)
                                        throwError e
                                  )
                $info "Check: {} (depth {}) => {}" (show smiMove, dpTarget smiDepth, show score)
                return score
     
     -- restrictInterval smiGlobalInterval smiSide score
     return (smiMove, score)

type DepthIterationInput = (AlphaBetaParams, TVar (Score, Score), [PossibleMove], Maybe DepthIterationOutput)
type DepthIterationOutput = [(PossibleMove, Score)]
type AiOutput = ([PossibleMove], Score)

rememberScoreShift :: AICacheHandle rules eval -> GameId -> ScoreBase -> Checkers ()
rememberScoreShift handle gameId shift = liftIO $ atomically $ do
  shifts <- readTVar (aichLastMoveScoreShift handle)
  let shifts' = M.insert gameId shift shifts
  writeTVar (aichLastMoveScoreShift handle) shifts'

getLastScoreShift :: AICacheHandle rules eval -> GameId -> Checkers (Maybe ScoreBase)
getLastScoreShift handle gameId = liftIO $ atomically $ do
  shifts <- readTVar (aichLastMoveScoreShift handle)
  return $ M.lookup gameId shifts

getPossibleMoves :: GameRules rules => AICacheHandle rules eval -> Side -> Board -> Checkers [PossibleMove]
getPossibleMoves handle side board = Monitoring.timed "ai.possible_moves.duration" $ do
    let rules = aichRules handle
    Monitoring.increment "ai.possible_moves.calls"
    return $ possibleMoves rules side board
--     (result, hit) <- liftIO $ do
--         let memo = aichPossibleMoves handle
--         let rules = aichRules handle
--         let moves = possibleMoves rules side board
--         mbItem <- lookupBoardMap memo board
--         case mbItem of
--             Nothing -> do
--               let value = case side of
--                            First -> (Just moves, Nothing) 
--                            Second -> (Nothing, Just moves)
--               putBoardMap memo board value
--               return (moves, False)
--             Just (Just cachedMoves, _) | side == First -> return (cachedMoves, True)
--             Just (_, Just cachedMoves) | side == Second -> return (cachedMoves, True)
--             Just (mbMoves1, mbMoves2) -> do
--               let value
--                    | side == First = (Just moves, mbMoves2)
--                    | otherwise     = (mbMoves1, Just moves)
--               putBoardMap memo board value
--               return (moves, False)
--     if hit
--       then Monitoring.increment "ai.possible_moves.hit"
--       else Monitoring.increment "ai.possible_moves.miss"
--     return result

-- | General driver / controller for Alpha-Beta prunning algorithm.
-- This method is responsible in running scoreAB method on all possible moves
-- and selecting the best move.
--
-- This is done, in general, in three stages:
--
-- 1. Preselect. From all possible moves, select ones that look good at a first glance.
--    This logic can be used to make AI work faster, but it obviously can miss some moves
--    that are not so good from a first glance, but are very good from the second glance.
-- 
-- 2. Depth-wise loop. Score all moves with specified depth. If there is still time, then
--    score them again with better depth. Repeat until there is still time.
--    Each iteration can be interrupted by TimeExhaused exception.
--    If last iteration was not interrupted, then use results of last iteration.
--    If last iteration was interrputed, then merge results of last iteration with results
--    of previous one: for moves that we was not able to calculate with better depth,
--    use results with previous depth.
--    If timeout is not specified, then only one iteration is executed, without timeout.
--    The depth to start with should not be very big, so that we should be always able to
--    calculate all moves with at least start depth. Neither should it be too small, 
--    otherwise we would re-calculate the same for many times.
--
-- 3. Width-wise loop. This is performed within each depth iteration.
--    Specifics of alpha-beta prunning algorithm is so that the lesser 
--    (alpha, beta) range is provided at start, the faster algorithm works; however,
--    in case real score is outside of these bounds, it will return eiter alpha or beta
--    value instead of real score value. So, we do the following:
--
--    *  Select initial "width range", which is range of scores (alpha, beta). This range
--       is selected based on evaluation of current board with zero depth, plus-minus some
--       small delta.
--       Run scoreAB in that range.
--    *  If values returned by scoreAB are within selected initial range, then everything is
--       okay: we just select the best of returned values.
--    *  If exactly one move seems to bee "too good", i.e. corresponding result of scoreAB
--       equals to alpha/beta (depending on side), then we do not bother about it's exact
--       score: we should do that move anyway.
--    *  If there are more than one "too good" moves, then we should select the next interval
--       (alpha, beta), and run the next iteration only on that moves that seem to be "too good".
--    *  If all moves seem to be "too bad", then we should select the previous interval of
--       (alpha, beta), and run the next iteration on all moves in that interval.
--    *  It is possible (not very likely, but possible) that real score of some moves equals
--       exactly to alpha or beta bound that we selected on some iteration. To prevent switching
--       between "better" and "worther" intervals forwards and backwards indefinitely, we
--       introduce a restriction: if we see that scoreAB returned the bound value, but we have
--       already considered the interval on that side, then we know that the real score equals
--       exactly to the bound.
--
runAI :: (GameRules rules, Evaluator eval)
      => AlphaBeta rules eval
      -> AICacheHandle rules eval
      -> GameId
      -> Side
      -> Board
      -> Checkers AiOutput
runAI ai@(AlphaBeta params rules eval) handle gameId side board = do
    preOptions <- preselect
    options <- depthDriver preOptions
    output <- select options
    let bestScore = sNumeric $ snd output
    let shift = bestScore - sNumeric score0
    rememberScoreShift handle gameId shift
    return output
  where
    maximize = side == First
    minimize = not maximize

    betterThan s1 s2
      | maximize = s1 > s2
      | otherwise = s1 < s2

    worseThan s1 s2 = not (betterThan s1 s2)

    preselect =
      getPossibleMoves handle side board

--     preselect :: Checkers [PossibleMove]
--     preselect = do
--       moves <- getPossibleMoves handle side board
--       if length moves <= abMovesHighBound params
--         then return moves
--         else do
--           let simple = DepthParams {
--                         dpTarget = 2
--                       , dpCurrent = -1
--                       , dpMax = 4
--                       , dpMin = 2
--                       , dpForcedMode = False
--                     }
--           $info "Preselecting; number of possible moves = {}, depth = {}" (length moves, dpTarget simple)
--           options <- scoreMoves' moves simple (loose, win)
--           let key = if maximize
--                       then negate . snd
--                       else snd
--           let sorted = sortOn key options
--               bestOptions = take (abMovesHighBound params) sorted
--           let result = map fst sorted
--           $debug "Pre-selected options: {}" (Single $ show result)
--           return result

    depthDriver :: [PossibleMove] -> Checkers DepthIterationOutput
    depthDriver moves = do
      globalInterval <- liftIO $ atomically $ newTVar (loose, win)
      case abBaseTime params of
        Nothing -> do
          (result, _) <- go (params, globalInterval, moves, Nothing)
          return result
        Just time -> repeatTimed' "runAI" time goTimed (params, globalInterval, moves, Nothing)
  
    goTimed :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    goTimed (params, globalInterval, moves, prevResult) = do
      ret <- tryC $ go (params, globalInterval, moves, prevResult)
      case ret of
        Right result -> return result
        Left TimeExhaused ->
          case prevResult of
            Just result -> return (result, Nothing)
            Nothing -> return ([(move, 0) | move <- moves], Nothing)
        Left err -> throwError err

    go :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    go (params, globalInterval, moves, prevResult) = do
      let depth = abDepth params
      if length moves <= 1 -- Just one move possible
        then do
          $info "There is only one move possible; just do it." ()
          return ([(move, score0) | move <- moves], Nothing)
                                                             
        else do
          let var = aichData handle
          $info "Selecting a move. Side = {}, depth = {}, number of possible moves = {}" (show side, depth, length moves)
          dp <- updateDepth params moves $ DepthParams {
                     dpTarget = depth
                   , dpCurrent = -1
                   , dpMax = abCombinationDepth params + depth
                   , dpMin = fromMaybe depth (abStartDepth params)
                   , dpForcedMode = False
                   }
          let needDeeper = abDeeperIfBad params && score0 `worseThan` 0
          let dp'
                | needDeeper = dp {
                                    dpTarget = min (dpMax dp) (dpTarget dp + 1)
                                  }
                | otherwise = dp
          result <- widthController True True prevResult moves dp' globalInterval =<< initInterval
          -- In some corner cases, there might be 1 or 2 possible moves,
          -- so the timeout would allow us to calculate with very big depth;
          -- too big depth does not decide anything in such situations.
          if depth < 50
            then do
              let params' = params {abDepth = depth + 1, abStartDepth = Nothing}
              return (result, Just (params', globalInterval, moves, Just result))
            else return (result, Nothing)

    score0 = evalBoard eval First board

    -- | Initial (alpha, beta) interval
    initInterval :: Checkers (Score, Score)
    initInterval = do
      let delta
            | abs score0 < 4 = 1
            | abs score0 < 8 = 2
            | otherwise = 4
      mbPrevShift <- getLastScoreShift handle gameId
      case mbPrevShift of
        Nothing -> do
            let alpha = score0 - delta
                beta  = score0 + delta
            $debug "Score0 = {}, delta = {} => initial interval ({}, {})" (score0, delta, alpha, beta)
            return (alpha, beta)
        Just shift -> do
            let (alpha, beta)
                  | shift >= 0 = (score0 - delta, score0 + (Score shift 0) + delta)
                  | otherwise  = (score0 + (Score shift 0) - delta, score0 + delta)
            $debug "Score0 = {}, delta = {}, shift in previous move = {} => initial interval ({}, {})"
                              (score0, delta, shift, alpha, beta)
            return (alpha, beta)

    selectScale :: Score -> ScoreBase
    selectScale s
      | s > 10000 = 1000
      | s > 1000 = 10
      | s > 100 = 5
      | otherwise = 2

    nextInterval :: Score -> (Score, Score) -> (Score, Score)
    nextInterval good (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scaleScore` width
          alpha' = min good (prevScore alpha)
          beta'  = max good (nextScore beta)
      in  if maximize
            then (beta', max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), alpha')

    prevInterval :: Score -> (Score, Score) -> (Score, Score)
    prevInterval bad (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scaleScore` width
          alpha' = min bad (prevScore alpha)
          beta'  = max bad (nextScore beta)
      in  if minimize
            then (beta', max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), alpha')

    widthController :: Bool -- ^ Allow to shift (alpha,beta) segment to bigger values?
                    -> Bool -- ^ Allow to shift (alpha,beta) segment to lesser values?
                    -> Maybe DepthIterationOutput -- ^ Results of previous depth iteration
                    -> [PossibleMove]
                    -> DepthParams
                    -> TVar (Score, Score) -- ^ Global (alpha, beta)
                    -> (Score, Score) -- ^ (Alpha, Beta)
                    -> Checkers DepthIterationOutput
    widthController allowNext allowPrev prevResult moves dp globalInterval localInterval = do
      interval@(alpha, beta) <- getRestrictedInterval globalInterval localInterval
      if alpha == beta
        then do
          $info "Empty scores interval: [{}]. We have to think that all moves have this score." (Single alpha)
          return [(move, alpha) | move <- moves]
        else do
            results <- widthIteration prevResult moves dp globalInterval interval
            let (goodScore, good, badScore, badMoves) = selectBestEdge interval moves results
                (bestMoves, bestResults) = unzip good
            if length badMoves == length moves
              then
                if allowPrev
                  then do
                    let interval' = prevInterval badScore interval
                    $info "All moves are `too bad'; consider worse scores interval: [{} - {}]" interval'
                    widthController False True prevResult badMoves dp globalInterval interval'
                  else do
                    $info "All moves are `too bad' ({}), but we have already checked worse interval; so this is the real score." (Single badScore)
                    return [(move, badScore) | move <- moves]
              else
                case bestResults of
                  [] -> return results
                  [_] -> do
                    $info "Exactly one move is `too good'; do that move." ()
                    return bestResults
                  _ ->
                    if allowNext
                      then do
                        let interval'@(alpha',beta') = nextInterval goodScore interval
                        $info "Some moves ({} of them) are `too good'; consider better scores interval: [{} - {}]" (length bestMoves, alpha', beta')
                        widthController True False prevResult bestMoves dp globalInterval interval'
                      else do
                        $info  "Some moves ({} of them) are `too good'; but we have already checked better interval; so this is the real score" (Single $ length bestMoves)
                        return bestResults

    scoreMoves :: [PossibleMove]
               -> DepthParams
               -> TVar (Score, Score) -- ^ Global interval
               -> (Score, Score)      -- ^ Local interval
               -> Checkers [Either Error (PossibleMove, Score)]
    scoreMoves moves dp globalInterval (localAlpha, localBeta) = do
      let var = aichData handle
      let processor = aichProcessor handle
      let inputs = [
            ScoreMoveInput {
              smiAi = ai,
              smiCache = handle,
              smiGameId = gameId,
              smiSide = side,
              smiDepth = dp,
              smiBoard = board,
              smiMove = move,
              smiGlobalInterval = globalInterval,
              smiAlpha = localAlpha,
              smiBeta = localBeta
            } | move <- moves ]
      process' processor inputs
    
    scoreMoves' :: [PossibleMove]
                -> DepthParams
                -> TVar (Score, Score)
                -> (Score, Score)
                -> Checkers DepthIterationOutput
    scoreMoves' moves dp globalInterval localInterval = do
      results <- scoreMoves moves dp globalInterval localInterval
      case sequence results of
        Right result -> return result
        Left err -> throwError err

    widthIteration :: Maybe DepthIterationOutput
                  -> [PossibleMove]
                  -> DepthParams
                  -> TVar (Score, Score)
                  -> (Score, Score)
                  -> Checkers DepthIterationOutput
    widthIteration prevResult moves dp globalInterval localInterval = do
      (alpha, beta) <- getRestrictedInterval globalInterval localInterval
      $info "`- Considering scores interval: [{} - {}], depth = {}" (alpha, beta, dpTarget dp)
      results <- scoreMoves moves dp globalInterval (alpha, beta)
      joinResults prevResult results

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
          goodResults = [(move, (goodMoves, score)) | (move, (goodMoves, score)) <- zip moves results, score >= good]
          badResults = [move | (move, (_, score)) <- zip moves results, score <= bad]
          scores = map snd results
          badScore = if maximize
                       then minimum scores
                       else maximum scores
          goodScore = if maximize
                        then maximum scores
                        else minimum scores
      in  (goodScore, goodResults, bad, badResults)

    select :: DepthIterationOutput -> Checkers AiOutput
    select pairs = do
      let best = if maximize then maximum else minimum
          maxScore = best $ map snd pairs
          goodMoves = [move | (move, score) <- pairs, score == maxScore]
      return (goodMoves, maxScore)

-- | Calculate score of the board
doScore :: (GameRules rules, Evaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> GameId
        -> Side
        -> DepthParams
        -> Board
        -> TVar (Score, Score)
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers Score
doScore rules eval var params gameId side dp board globalInterval alpha beta = do
    initState <- mkInitState
    out <- evalStateT (cachedScoreAB var params input) initState
    return $ soScore out
  where
    input = ScoreInput side dp alpha beta board Nothing 
    mkInitState = do
      now <- liftIO $ getTime Monotonic
      let timeout = case abBaseTime params of
                      Nothing -> Nothing
                      Just sec -> Just $ TimeSpec (fromIntegral sec) 0
      return $ ScoreState rules eval gameId globalInterval [loose] now timeout

-- | State of ScoreM monad.
data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssGameId :: GameId
  , ssGlobalInterval :: TVar (Score, Score)
  , ssBestScores :: [Score] -- ^ At each level of depth-first search, there is own "best score"
  , ssStartTime :: TimeSpec -- ^ Start time of calculation
  , ssTimeout :: Maybe TimeSpec -- ^ Nothing for "no timeout"
  }

-- | Input data for scoreAB method.
data ScoreInput = ScoreInput {
    siSide :: Side
  , siDepth :: DepthParams
  , siAlpha :: Score
  , siBeta :: Score
  , siBoard :: Board
  , siPrevMove :: Maybe PossibleMove
  }

data ScoreOutput = ScoreOutput {
    soScore :: Score
  , soQuiescene :: Bool
  }

-- | ScoreM monad.
type ScoreM rules eval a = StateT (ScoreState rules eval) Checkers a

instance HasMetricsConfig (StateT (ScoreState rules eval) Checkers) where
  isMetricsEnabled = lift isMetricsEnabled

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

clamp :: Ord a => a -> a -> a -> a
clamp alpha beta score
  | score < alpha = alpha
  | score > beta  = beta
  | otherwise = score

restrictInterval :: MonadIO m => TVar (Score, Score) -> Side -> Score -> m ()
restrictInterval var side score = liftIO $ atomically $ do
  (globalAlpha, globalBeta) <- readTVar var
  when (globalAlpha < score && score < globalBeta) $
    if side == First -- maximize
      then writeTVar var (score, globalBeta)
      else writeTVar var (globalAlpha, score)

getRestrictedInterval :: (MonadIO m, HasLogger m, HasLogContext m) => TVar (Score, Score) -> (Score, Score) -> m (Score, Score)
getRestrictedInterval global (localAlpha, localBeta) = do
  (globalAlpha, globalBeta) <- liftIO $ atomically $ readTVar global
  let alpha1 = max globalAlpha localAlpha
      beta1  = min globalBeta  localBeta
  if  alpha1 <= beta1
    then do
         $trace "Restrict: Global [{}, {}] x Local [{}, {}] => [{}, {}]"
                  (globalAlpha, globalBeta, localAlpha, localBeta, alpha1, beta1)
         return (alpha1, beta1)
    else do
         let mid = (alpha1 + beta1) `divideScore` 2
         return (mid, mid)

-- | Calculate score of the board. 
-- This uses the cache. It is called in the recursive call also.
cachedScoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
              => AICacheHandle rules eval
              -> AlphaBetaParams
              -> ScoreInput
              -> ScoreM rules eval ScoreOutput
cachedScoreAB var params input = do
  let depth = dpCurrent dp
      side = siSide input
      board = siBoard input
      dp = siDepth input
      alpha = siAlpha input
      beta = siBeta input
  mbItem <- lift $ lookupAiCache params board dp var
  mbCached <- case mbItem of
                Just item -> do
                  let score = itemScore item
                  -- it is possible that this value was put to cache with different
                  -- values of alpha/beta; but we have to maintain the property of
                  -- AB-section: alpha <= result <= beta. So here we clamp the value
                  -- that we got from cache.
                  case itemBound item of
                    Exact -> return $ Just $ ScoreOutput score False
                    Alpha -> if score <= alpha
                               then return $ Just $ ScoreOutput alpha False
                               else return Nothing
                    Beta  -> if score >= beta
                               then return $ Just $ ScoreOutput beta False
                               else return Nothing
                Nothing -> return Nothing
  case mbCached of
    Just out -> return out
    Nothing -> do
      out <- Monitoring.timed "ai.score.board" $ scoreAB var params input
      let score = soScore out
          bound
            | score <= alpha = Alpha
            | score >= beta = Beta
            | otherwise = Exact
          -- we can only put the result to the cache if we know
          -- that this score was not clamped by alpha or beta
          -- (so this is a real score, not alpha/beta bound)
          item = PerBoardData (dpLast dp) score bound Nothing
          item' = PerBoardData (dpLast dp) (negate score) bound Nothing
      when (bound == Exact && soQuiescene out) $ do
          lift $ putAiCache params board item var
          lift $ putAiCache params (flipBoard board) item' var
      return out

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
updateDepth :: (Monad m, HasLogging m, MonadIO m) => AlphaBetaParams -> [PossibleMove] -> DepthParams -> m DepthParams
updateDepth params moves dp
    | forced = do
                  let delta = nMoves - 1
                  let target = min (dpTarget dp + 1) (dpMax dp - delta)
                  let indent = replicate (2*dpCurrent dp) ' '
                  $trace "{}| there is only one move, increase target depth to {}"
                          (indent, target)
                  return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target, dpForcedMode = True}
    | nMoves > abMovesHighBound params && isQuiescene moves = do
                  let target = max (dpCurrent dp + 1) (dpMin dp)
                  let indent = replicate (2*dpCurrent dp) ' '
                  $trace "{}| there are too many moves, decrease target depth to {}"
                          (indent, target)
                  return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target}
    | otherwise = return $ dp {dpCurrent = dpCurrent dp + 1}
  where
    nMoves = length moves
    forced = any isCapture moves || any isPromotion moves || nMoves <= abMovesLowBound params

isQuiescene :: [PossibleMove] -> Bool
isQuiescene moves = not (any isCapture moves || any isPromotion moves)

-- | Check if timeout is exhaused.
isTimeExhaused :: ScoreM rules eval Bool
isTimeExhaused = do
  check <- gets ssTimeout
  case check of
    Nothing -> return False
    Just delta -> do
      start <- gets ssStartTime
      now <- liftIO $ getTime Monotonic
      return $ start + delta <= now

-- | Calculate score for the board.
-- This implements the alpha-beta section algorithm itself.
scoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
        => AICacheHandle rules eval
        -> AlphaBetaParams
        -> ScoreInput
        -> ScoreM rules eval ScoreOutput
scoreAB var params input
  | isTargetDepth dp = do
      -- target depth is achieved, calculate score of current board directly
      evaluator <- gets ssEvaluator
      let score0 = evalBoard' evaluator board
      (alpha, beta) <- getRestrictedInterval'
      $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      quiescene <- checkQuiescene
      return $ ScoreOutput score0 quiescene
  | otherwise = do
      -- first, let "best" be the worse possible value
      let best = if maximize then loose else win -- we assume alpha <= beta
      (alpha, beta) <- getRestrictedInterval'
      if alpha == beta
        then do
            quiescene <- checkQuiescene
            return $ ScoreOutput best quiescene
        else do
          push best
          $trace "{}V Side: {}, A = {}, B = {}" (indent, show side, show alpha, show beta)
          rules <- gets ssRules
          moves <- lift $ getPossibleMoves var side board

          -- this actually means that corresponding side lost.
          when (null moves) $
            $trace "{}`—No moves left." (Single indent)

          dp' <- updateDepth params moves dp
          let prevMove = siPrevMove input
          moves' <- sortMoves prevMove moves
          out <- iterateMoves (zip [1..] moves') dp'
          pop
          return out

  where

    side = siSide input
    dp = siDepth input
    localAlpha = siAlpha input
    localBeta = siBeta input
    board = siBoard input

    getRestrictedInterval' = do
      globalInterval <- gets ssGlobalInterval
      result@(alpha, beta) <- getRestrictedInterval globalInterval (localAlpha, localBeta)
      return result


    evalBoard' :: eval -> Board -> Score
    evalBoard' evaluator board = result
      where
        score = evalBoard evaluator First board
        result
          | maximize && sNumeric score == sNumeric win   = score - Score 0 (fromIntegral $ dpCurrent dp)
          | minimize && sNumeric score == sNumeric loose = score + Score 0 (fromIntegral $ dpCurrent dp)
          | otherwise = score

    checkQuiescene :: ScoreM rules eval Bool
    checkQuiescene = do
      rules <- gets ssRules
      moves <- lift $ getPossibleMoves var (opposite side) board
      return $ isQuiescene moves

    push :: Score -> ScoreM rules eval ()
    push score =
      modify $ \st -> st {ssBestScores = score : ssBestScores st}

    pop :: ScoreM rules eval ()
    pop =
      modify $ \st -> st {ssBestScores = tail (ssBestScores st)}

    evalMove :: Maybe PossibleMove -> PossibleMove -> ScoreM rules eval Int
    evalMove mbPrevMove move = do
      let victims = pmVictims move
          nVictims = length victims
          promotion = if isPromotion move then 1 else 0
          attackPrevPiece = case mbPrevMove of
                              Nothing -> 0
                              Just prevMove -> if pmEnd prevMove `elem` victims
                                                 then 2
                                                 else 0

      let board' = applyMoveActions (pmResult move) board
      let dp0 = dp {dpCurrent = dpTarget dp}
      mbCached <- lift $ lookupAiCache params board' dp0 var
      let primeVariation = case mbCached of
                             Nothing -> 0
                             Just item ->
                              let score = sNumeric (itemScore item)
                                  scoreSigned = if maximize then score else negate score
                              in  fromIntegral $ 1 + 5 * scoreSigned
        
      return $ nVictims + promotion + attackPrevPiece + primeVariation

    sortMoves :: Maybe PossibleMove -> [PossibleMove] -> ScoreM rules eval [PossibleMove]
    sortMoves mbPrevMove moves =
      if length moves >= abMovesHighBound params
        then do
          interest <- mapM (evalMove mbPrevMove) moves
          if any (> 0) interest
            then return $ map fst $ sortOn (negate . snd) $ zip moves interest
            else return moves
        else return moves

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

    opponentMoves :: ScoreM rules eval [PossibleMove]
    opponentMoves = do
      rules <- gets ssRules
      lift $ getPossibleMoves var (opposite side) board

    isInteresting move = do
      opMoves <- opponentMoves
      let victims = concatMap pmVictims opMoves
      return $ {- pmBegin move `elem` victims || -} length (pmVictims move) >= 2 || isPromotion move

    mkIntervals (alpha, beta) =
      let mid = (alpha + beta) `divideScore` 2
      in  if maximize
            then [(alpha, prevScore mid), (mid, beta)]
            else [(mid, beta), (alpha, nextScore mid)]

    checkMove :: AICacheHandle rules eval -> AlphaBetaParams -> ScoreInput -> PossibleMove -> ScoreM rules eval ScoreOutput
    checkMove var params input move = do
        let alpha = siAlpha input
            beta  = siBeta input
            width = beta - alpha
        intervals <- do
              interesting <- isInteresting move
              if interesting || width <= 2
                then return [(alpha, beta)]
                else return $ mkIntervals (alpha, beta)
        let inputs = [input {siAlpha = alpha, siBeta = beta} | (alpha, beta) <- intervals]
        go inputs
      where
        go [input] = cachedScoreAB var params input
        go (input : inputs) = do
          out <- cachedScoreAB var params input
          let score = soScore out
          (alpha, beta) <- getRestrictedInterval'
          if maximize && score >= beta || minimize && score <= alpha
            then go inputs
            else return out


    iterateMoves :: [(Int,PossibleMove)] -> DepthParams -> ScoreM rules eval ScoreOutput
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}`—All moves considered at this level, return best = {}" (indent, show best)
      quiescene <- checkQuiescene
      return $ ScoreOutput best quiescene
    iterateMoves ((i,move) : moves) dp = do
      timeout <- isTimeExhaused
      when timeout $ do
        -- $info "Timeout exhaused for depth {}." (Single $ dpCurrent dp)
        throwError TimeExhaused
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      evaluator <- gets ssEvaluator
      rules <- gets ssRules
      best <- getBest
      let (alpha, beta) = (localAlpha, localBeta)
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
                    , siDepth = dp
                  }
      out <- cachedScoreAB var params input'
      let score = soScore out
      $trace "{}| score for side {}: {}" (indent, show side, show score)

      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if (maximize && score >= beta) || (minimize && score <= alpha)
               then do
                    Monitoring.distribution "ai.section.at" $ fromIntegral i
                    $trace "{}`—Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, show score)
                    quiescene <- checkQuiescene
                    return $ ScoreOutput score quiescene
                    
               else iterateMoves moves dp
        else do
             iterateMoves moves dp
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks board =
    evalBoard eval whoAsks board

