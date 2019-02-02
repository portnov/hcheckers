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

module AI.AlphaBeta where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM
import Data.Maybe
import Data.List (sortOn)
import qualified Data.Map as M
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
    liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ boardCounts board
    return moves

  initAi ai@(AlphaBeta _ rules eval) cache gameId board side =
    setTree cache gameId $ mkTree rules board side

  updateAi ai@(AlphaBeta _ rules eval) json =
    case fromJSON json of
      Error _ -> ai
      Success params -> AlphaBeta params rules (updateEval eval json)

  aiName _ = "default"

mkTree :: GameRules rules => rules -> Board -> Side -> Tree
mkTree rules board0 first =
  let children side node = flip map (possibleMoves rules side (nodeBoard node)) $ \move ->
          let side' = opposite side
              child = Node board' side' $ children side' child
              board' = applyMoveActions (pmResult move) (nodeBoard node)
          in  (move, child)
      root = Node board0 first $ children first root
  in  root

getTree :: AICacheHandle rules eval -> GameId -> Checkers Tree
getTree handle gameId = liftIO $ atomically $ do
    nodes <- readTVar $ aichCurrentNodes handle
    case M.lookup gameId nodes of
      Just tree -> return tree
      Nothing -> fail "Cant obtain current game tree node"

setTree :: AICacheHandle rules eval -> GameId -> Tree -> Checkers ()
setTree handle gameId tree = liftIO $ atomically $ do
    nodes <- readTVar $ aichCurrentNodes handle
    writeTVar (aichCurrentNodes handle) $ M.insert gameId tree nodes

findTree :: AICacheHandle rules eval -> GameId -> Board -> Checkers Tree
findTree handle gameId board = do
    currentRoot <- getTree handle gameId
    return $ find currentRoot
  where
    find root =
      -- let nodes = map snd $ concatMap (nodeChildren . snd) (nodeChildren root)
      let nodes = map snd $ nodeChildren root
      in case go nodes of
          Just node -> node
          Nothing -> error $ "Cant find specified board in the tree: " ++ show (map nodeBoard nodes)

    go [] = Nothing
    go (node : rest)
      | nodeBoard node == board = Just node
      | otherwise = go rest

-- | Calculate score of one possible move.
scoreMove :: (GameRules rules, Evaluator eval) => ScoreMoveInput rules eval -> Checkers (PossibleMove, Tree, Score)
scoreMove (ScoreMoveInput {..}) = do
     let AlphaBeta params rules eval = smiAi
     score <- Monitoring.timed "ai.score.move" $ do
                let board' = nodeBoard smiNode
                    side = nodeSide smiNode
                score <- doScore rules eval smiCache params smiGameId smiDepth smiNode smiAlpha smiBeta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show smiMove, dpTarget smiDepth, show e)
                                        throwError e
                                  )
                $info "Check: {} (depth {}) => {}" (show smiMove, dpTarget smiDepth, show score)
                return score
     
     return (smiMove, smiNode, score)

type DepthIterationInput = (AlphaBetaParams, [(PossibleMove, Tree)], Maybe DepthIterationOutput)
type DepthIterationOutput = [(PossibleMove, Tree, Score)]
type AiOutput = ([PossibleMove], Score)

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
    select options
  where
    maximize = side == First
    minimize = not maximize

    betterThan s1 s2
      | maximize = s1 > s2
      | otherwise = s1 < s2

    worseThan s1 s2 = not (betterThan s1 s2)

--     preselect =
--       return $ possibleMoves rules side board

    preselect :: Checkers [(PossibleMove, Tree)]
    preselect = do
      tree <- findTree handle gameId board
      let next = nodeChildren tree
      if length next <= abMovesHighBound params
        then return next
        else do
          let simple = DepthParams {
                        dpTarget = 2
                      , dpCurrent = -1
                      , dpMax = 4
                      , dpMin = 2
                    }
          $info "Preselecting; number of possible moves = {}, depth = {}" (length next, dpTarget simple)
          options <- scoreMoves' next simple (loose, win)
          let key = if maximize
                      then \(pm, node, score) -> negate score
                      else \(pm, node, score) -> score
          let sorted = sortOn key options
              bestOptions = take (abMovesHighBound params) sorted
          let result = [(move, node) | (move, node, score) <- sorted]
          $debug "Pre-selected options: {}" (Single $ show $ map fst result)
          return result

    depthDriver :: [(PossibleMove, Tree)] -> Checkers DepthIterationOutput
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
            Nothing -> return ([(move, node, 0) | (move, node) <- moves], Nothing)
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
          return ([(move, node', 0) | (move, node') <- moves], Nothing)
                                                             
        else do
          let var = aichData handle
          $info "Selecting a move. Side = {}, depth = {}, number of possible moves = {}" (show side, depth, length moves)
          dp <- updateDepth params moves $ DepthParams {
                     dpTarget = depth
                   , dpCurrent = -1
                   , dpMax = abCombinationDepth params + depth
                   , dpMin = fromMaybe depth (abStartDepth params)
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
                    -> [(PossibleMove, Tree)]
                    -> DepthParams
                    -> (Score, Score) -- ^ (Alpha, Beta)
                    -> Checkers DepthIterationOutput
    widthController allowNext allowPrev prevResult options dp interval@(alpha,beta) =
      if alpha == beta
        then do
          $info "Empty scores interval: [{}]. We have to think that all options have this score." (Single alpha)
          return [(move, node', alpha) | (move, node') <- options]
        else do
            results <- widthIteration prevResult options dp interval
            -- results :: [(PossibleMove, Tree, Score)]
            -- options :: [(PossibleMove, Score)]
            let (good, badScore, badMoves) = selectBestEdge interval options results
                (bestMoves, bestResults) = unzip good
            if length badMoves == length options
              then
                if allowPrev
                  then do
                    let interval' = prevInterval interval
                    $info "All moves are `too bad'; consider worse scores interval: [{} - {}]" interval'
                    widthController False True prevResult badMoves dp interval'
                  else do
                    $info "All moves are `too bad' ({}), but we have already checked worse interval; so this is the real score." (Single badScore)
                    return [(move, node, badScore) | (move, node) <- options]
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

    scoreMoves :: [(PossibleMove, Tree)]
               -> DepthParams
               -> (Score, Score)
               -> Checkers [Either Error (PossibleMove, Tree, Score)]
    scoreMoves moves dp (alpha, beta) = do
      let var = aichData handle
      AICache _ processor _ <- liftIO $ atomically $ readTVar var
      let inputs = [
            ScoreMoveInput {
              smiAi = ai,
              smiCache = handle,
              smiGameId = gameId,
              smiDepth = dp,
              smiMove = move,
              smiNode = node,
              smiAlpha = alpha,
              smiBeta = beta
            } | (move, node) <- moves ]
      process' processor inputs
    
    scoreMoves' :: [(PossibleMove, Tree)] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    scoreMoves' moves dp (alpha, beta) = do
      results <- scoreMoves moves dp (alpha, beta)
      case sequence results of
        Right result -> return result
        Left err -> throwError err

    widthIteration :: Maybe DepthIterationOutput -> [(PossibleMove, Tree)] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    widthIteration prevResult moves dp (alpha, beta) = do
      $info "`- Considering scores interval: [{} - {}], depth = {}" (alpha, beta, dpTarget dp)
      results <- scoreMoves moves dp (alpha, beta)
      joinResults prevResult results

    joinResults :: Maybe DepthIterationOutput -> [Either Error (PossibleMove, Tree, Score)] -> Checkers DepthIterationOutput
    joinResults Nothing results =
      case sequence results of
        Right result -> return result
        Left err -> throwError err
    joinResults (Just prevResults) results = zipWithM joinResult prevResults results

    joinResult :: (PossibleMove, Tree, Score) -> Either Error (PossibleMove, Tree, Score) -> Checkers (PossibleMove, Tree, Score)
    joinResult prev@(move, node, score) (Left TimeExhaused) = do
      $info "Time exhaused while checking move {}, use result from previous depth: {}" (show move, score)
      return prev
    joinResult _ (Left err) = throwError err
    joinResult _ (Right result) = return result

    --selectBestEdge :: (Score, Score) -> [(PossibleMove, Score)] -> [(PossibleMove, Tree, Score)]
    selectBestEdge (alpha, beta) options results =
      let (good, bad) = if maximize then (beta, alpha) else (alpha, beta)
          goodResults = [((move, node), (goodMoves, node, score)) | ((move, node), (goodMoves, _, score)) <- zip options results, score == good]
          badResults = [move | (move, (_, _, score)) <- zip options results, score == bad]
      in  (goodResults, bad, badResults)

    select :: DepthIterationOutput -> Checkers AiOutput
    select options = do
      let best = if maximize then maximum else minimum
          maxScore = best [score | (move, node, score) <- options]
          goodMoves = [move | (move, node, score) <- options, score == maxScore]
      return (goodMoves, maxScore)

-- | Calculate score of the board
doScore :: (GameRules rules, Evaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> GameId
        -> DepthParams
        -> Tree
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers Score
doScore rules eval var params gameId dp node alpha beta = do
    initState <- mkInitState
    out <- evalStateT (cachedScoreAB var params input) initState
    return $ soScore out
  where
    input = ScoreInput dp node alpha beta Nothing 
    mkInitState = do
      now <- liftIO $ getTime Monotonic
      let timeout = case abBaseTime params of
                      Nothing -> Nothing
                      Just sec -> Just $ TimeSpec (fromIntegral sec) 0
      return $ ScoreState rules eval gameId [loose] now timeout

-- | State of ScoreM monad.
data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssGameId :: GameId
  , ssBestScores :: [Score] -- ^ At each level of depth-first search, there is own "best score"
  , ssStartTime :: TimeSpec -- ^ Start time of calculation
  , ssTimeout :: Maybe TimeSpec -- ^ Nothing for "no timeout"
  }

-- | Input data for scoreAB method.
data ScoreInput = ScoreInput {
    siDepth :: DepthParams
  , siNode :: Tree
  , siAlpha :: Score
  , siBeta :: Score
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

-- | Calculate score of the board. 
-- This uses the cache. It is called in the recursive call also.
cachedScoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
              => AICacheHandle rules eval
              -> AlphaBetaParams
              -> ScoreInput
              -> ScoreM rules eval ScoreOutput
cachedScoreAB var params input = do
  let depth = dpCurrent dp
      side = nodeSide $ siNode input
      board = nodeBoard $ siNode input
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
        then return $ ScoreOutput alpha False
        else if score > beta
               then return $ ScoreOutput beta False
               else return $ ScoreOutput score False

    Nothing -> do
      out <- Monitoring.timed "ai.score.board" $ scoreAB var params input
      let score = soScore out
      when (alpha < score && score < beta && soQuiescene out) $
          -- we can only put the result to the cache if we know
          -- that this score was not clamped by alpha or beta
          -- (so this is a real score, not alpha/beta bound)
          lift $ putAiCache params board dp side score [] var
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
updateDepth :: (Monad m, HasLogging m, MonadIO m) => AlphaBetaParams -> [(PossibleMove, Tree)] -> DepthParams -> m DepthParams
updateDepth params options dp
    | forced = do
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
  where
    moves = map fst options
    nMoves = length options
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
      let nextNodes = nodeChildren $ siNode input

      -- this actually means that corresponding side lost.
      when (null nextNodes) $
        $trace "{}`—No moves left." (Single indent)

      dp' <- updateDepth params nextNodes dp
      let prevMove = siPrevMove input
      out <- iterateMoves (sortMoves nextNodes) dp'
      pop
      return out

  where

    side = nodeSide $ siNode input
    board = nodeBoard $ siNode input
    dp = siDepth input
    alpha = siAlpha input
    beta = siBeta input

    checkQuiescene :: ScoreM rules eval Bool
    checkQuiescene = do
      rules <- gets ssRules
      return $ isQuiescene $ possibleMoves rules (opposite side) board

    push :: Score -> ScoreM rules eval ()
    push score =
      modify $ \st -> st {ssBestScores = score : ssBestScores st}

    pop :: ScoreM rules eval ()
    pop =
      modify $ \st -> st {ssBestScores = tail (ssBestScores st)}

    sortMoves :: [(PossibleMove, Tree)] -> [(PossibleMove, Tree)]
    sortMoves moves =
      let promotions = map (isPromotion . fst) moves
      in  if or promotions
            then map fst $ sortOn (not . snd) $ zip moves promotions
            else moves

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

    iterateMoves :: [(PossibleMove, Tree)] -> DepthParams -> ScoreM rules eval ScoreOutput
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}`—All moves considered at this level, return best = {}" (indent, show best)
      quiescene <- checkQuiescene
      return $ ScoreOutput best quiescene
    iterateMoves ((move, nextNode) : rest) dp' = do
      timeout <- isTimeExhaused
      when timeout $ do
        -- $info "Timeout exhaused for depth {}." (Single $ dpCurrent dp)
        throwError TimeExhaused
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      evaluator <- gets ssEvaluator
      rules <- gets ssRules
      best <- getBest
      let input' = input {
                      siAlpha = if maximize
                                 then max alpha best
                                 else alpha
                    , siBeta = if maximize
                                 then beta
                                 else min beta best
                    , siPrevMove = Just move
                    , siNode = nextNode
                    , siDepth = dp'
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
                    
               else iterateMoves rest dp'
        else do
             iterateMoves rest dp'
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks whoMovesNext board =
    evalBoard eval whoAsks whoMovesNext board

