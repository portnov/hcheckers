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

  chooseMove ai storage side board = do
    (moves, _) <- runAI ai storage side board
    liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ boardCounts board
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
                score <- doScore rules eval smiCache params (opposite smiSide) smiDepth board' smiAlpha smiBeta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show smiMove, dpTarget smiDepth, show e)
                                        throwError e
                                  )
                $info "Check: {} (depth {}) => {}" (show smiMove, dpTarget smiDepth, show score)
                return score
     
     return (smiMove, score)

type DepthIterationInput = (AlphaBetaParams, [PossibleMove], Maybe DepthIterationOutput)
type DepthIterationOutput = [(PossibleMove, Score)]
type AiOutput = ([PossibleMove], Score)

getPossibleMoves :: GameRules rules => AICacheHandle rules eval -> Side -> Board -> Checkers [PossibleMove]
getPossibleMoves handle side board = Monitoring.timed "ai.possible_moves.duration" $ do
    let rules = aichRules handle
    Monitoring.increment "ai.possible_moves.calls"
    return $ possibleMoves rules side board
--     (result, hit) <- liftIO $ do
--         let memo = aichPossibleMoves handle
--         let rules = aichRules handle
--         let bc = boardCounts board
--             bk = boardKey board
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
runAI :: (GameRules rules, Evaluator eval) => AlphaBeta rules eval -> AICacheHandle rules eval -> Side -> Board -> Checkers AiOutput
runAI ai@(AlphaBeta params rules eval) handle side board = do
    preOptions <- preselect
    options <- depthDriver preOptions
    select options
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
    depthDriver moves =
      case abBaseTime params of
        Nothing -> do
          (result, _) <- go (params, moves, Nothing)
          return result
        Just time -> repeatTimed' "runAI" time goTimed (params, moves, Nothing)
  
    goTimed :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    goTimed (params, moves, prevResult) = do
      ret <- tryC $ go (params, moves, prevResult)
      case ret of
        Right result -> return result
        Left TimeExhaused ->
          case prevResult of
            Just result -> return (result, Nothing)
            Nothing -> return ([(move, 0) | move <- moves], Nothing)
        Left err -> throwError err

    go :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    go (params, moves, prevResult) = do
      let depth = abDepth params
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
          result <- widthController True True prevResult moves dp' initInterval
          -- In some corner cases, there might be 1 or 2 possible moves,
          -- so the timeout would allow us to calculate with very big depth;
          -- too big depth does not decide anything in such situations.
          if depth < 50
            then do
              let params' = params {abDepth = depth + 1, abStartDepth = Nothing}
              return (result, Just (params', moves, Just result))
            else return (result, Nothing)

    score0 = evalBoard eval First side board

    -- | Initial (alpha, beta) interval
    initInterval :: (Score, Score)
    initInterval =
      let delta
            | abs score0 < 4 = 1
            | abs score0 < 8 = 2
            | otherwise = 4
      in  (score0 - delta, score0 + delta)
--       in  if maximize
--             then (prevScore score0, score0 + delta)
--             else (score0 - delta, nextScore score0)

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
    widthController allowNext allowPrev prevResult moves dp interval@(alpha,beta) =
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
                  _ ->
                    if allowNext
                      then do
                        let interval'@(alpha',beta') = nextInterval interval
                        $info "Some moves ({} of them) are `too good'; consider better scores interval: [{} - {}]" (length bestMoves, alpha', beta')
                        widthController True False prevResult bestMoves dp interval'
                      else do
                        $info  "Some moves ({} of them) are `too good'; but we have already checked better interval; so this is the real score" (Single $ length bestMoves)
                        return bestResults

    scoreMoves :: [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers [Either Error (PossibleMove, Score)]
    scoreMoves moves dp (alpha, beta) = do
      let var = aichData handle
      let processor = aichProcessor handle
      let inputs = [
            ScoreMoveInput {
              smiAi = ai,
              smiCache = handle,
              smiSide = side,
              smiDepth = dp,
              smiBoard = board,
              smiMove = move,
              smiAlpha = alpha,
              smiBeta = beta
            } | move <- moves ]
      process' processor inputs
    
    scoreMoves' :: [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    scoreMoves' moves dp (alpha, beta) = do
      results <- scoreMoves moves dp (alpha, beta)
      case sequence results of
        Right result -> return result
        Left err -> throwError err

    widthIteration :: Maybe DepthIterationOutput -> [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    widthIteration prevResult moves dp (alpha, beta) = do
      $info "`- Considering scores interval: [{} - {}], depth = {}" (alpha, beta, dpTarget dp)
      results <- scoreMoves moves dp (alpha, beta)
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
          goodResults = [(move, (goodMoves, score)) | (move, (goodMoves, score)) <- zip moves results, score == good]
          badResults = [move | (move, (_, score)) <- zip moves results, score == bad]
      in  (goodResults, bad, badResults)

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
        -> Side
        -> DepthParams
        -> Board
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers Score
doScore rules eval var params side dp board alpha beta = do
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
      return $ ScoreState rules eval [loose] now timeout

-- | State of ScoreM monad.
data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
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
  mbItem <- lift $ lookupAiCache params board dp side var
  mbCached <- case mbItem of
                Just item -> do
                  let score = cisScore item
                  -- it is possible that this value was put to cache with different
                  -- values of alpha/beta; but we have to maintain the property of
                  -- AB-section: alpha <= result <= beta. So here we clamp the value
                  -- that we got from cache.
                  case cisBound item of
                    Exact -> return $ Just $ ScoreOutput (clamp alpha beta score) False
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
          item = CacheItemSide score bound
      when (bound == Exact && soQuiescene out) $
          lift $ putAiCache params board dp side item [] var
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
                  $debug "{}| there is only one move, increase target depth to {}"
                          (indent, target)
                  return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target, dpForcedMode = True}
    | nMoves > abMovesHighBound params = do
                  let target = max (dpCurrent dp + 1) (dpMin dp)
                  let indent = replicate (2*dpCurrent dp) ' '
                  $debug "{}| there are too many moves, decrease target depth to {}"
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
      let score0 = evalBoard evaluator First side board
      $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      quiescene <- checkQuiescene
      return $ ScoreOutput score0 quiescene
  | otherwise = do
      -- first, let "best" be the worse possible value
      let best = if maximize then alpha else beta -- we assume alpha <= beta
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
      out <- iterateMoves moves' dp'
      pop
      return out

  where

    side = siSide input
    dp = siDepth input
    alpha = siAlpha input
    beta = siBeta input
    board = siBoard input

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
          promotion = if isPromotion move then 2 else 0
          attackPrevPiece = case mbPrevMove of
                              Nothing -> 0
                              Just prevMove -> if pmEnd prevMove `elem` victims
                                                 then 2
                                                 else 0
      return $ nVictims + promotion + attackPrevPiece

    sortMoves :: Maybe PossibleMove -> [PossibleMove] -> ScoreM rules eval [PossibleMove]
    sortMoves mbPrevMove moves = do
      interest <- mapM (evalMove mbPrevMove) moves
      if any (> 0) interest
        then return $ map fst $ sortOn (negate . snd) $ zip moves interest
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
          if maximize && score >= beta || minimize && score <= alpha
            then go inputs
            else return out


    iterateMoves :: [PossibleMove] -> DepthParams -> ScoreM rules eval ScoreOutput
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}`—All moves considered at this level, return best = {}" (indent, show best)
      quiescene <- checkQuiescene
      return $ ScoreOutput best quiescene
    iterateMoves (move : moves) dp = do
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
                    $trace "{}`—Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, show score)
                    quiescene <- checkQuiescene
                    return $ ScoreOutput score quiescene
                    
               else iterateMoves moves dp
        else do
             iterateMoves moves dp
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks whoMovesNext board =
    evalBoard eval whoAsks whoMovesNext board

