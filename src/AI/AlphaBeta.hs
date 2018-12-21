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

scoreMove :: (GameRules rules, Evaluator eval) => ScoreMoveInput rules eval -> Checkers (Move, Score)
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
     
     return (pmMove pm, score)

type AiIterationInput = (AlphaBetaParams, Maybe AiIterationOutput)
type AiIterationOutput = [(Move, Score)]
type AiOutput = ([Move], Score)

runAI :: (GameRules rules, Evaluator eval) => AlphaBeta rules eval -> AICacheHandle rules eval -> Side -> Board -> Checkers AiOutput
runAI ai@(AlphaBeta params rules eval) handle side board = do
    options <- depthDriver
    select options
  where
    depthDriver :: Checkers AiIterationOutput
    depthDriver = case abBaseTime params of
                 Nothing -> do
                    (result, _) <- go (params, Nothing)
                    return result
                 Just time -> repeatTimed' "runAI" time goTimed (params, Nothing)
  
    goTimed :: AiIterationInput
            -> Checkers (AiIterationOutput, Maybe AiIterationInput)
    goTimed (params, prevResult) = do
      ret <- tryC $ go (params, prevResult)
      case ret of
        Right result -> return result
        Left TimeExhaused ->
          case prevResult of
            Just result -> return (result, Nothing)
            Nothing -> do
              let moves = map pmMove $ possibleMoves rules side board
              return ([(move, 0) | move <- moves], Nothing)
        Left err -> throwError err

    go :: AiIterationInput
            -> Checkers (AiIterationOutput, Maybe AiIterationInput)
    go (params, prevResult) = do
      let depth = abDepth params
      let moves = possibleMoves rules side board
      if length moves <= 1 -- Just one move possible
        then do
          $info "There is only one move possible; just do it." ()
          -- currently we do not use results of evaluating of all moves
          -- when evaluating deeper parts of the tree (it is hard due to alpha-beta restrictions).
          -- It means we are not going to use that Score value anyway.
          return ([(pmMove move, 0) | move <- moves], Nothing)
                                                             
        else do
          let var = aichData handle
          $info "Evaluating a board, side = {}, depth = {}, number of possible moves = {}" (show side, depth, length moves)
          dp <- updateDepth (length moves) $ DepthParams {
                     dpTarget = depth
                   , dpCurrent = -1
                   , dpMax = abCombinationDepth params + depth
                   , dpMin = fromMaybe depth (abStartDepth params)
                   }
          joined <- widthController prevResult moves dp initInterval
          let params' = params {abDepth = depth + 1, abStartDepth = Nothing}
          return (joined, Just (params', Just joined))

    initInterval =
      let score0 = evalBoard eval First side board
          delta = 24
      in  (score0 - delta, score0 + delta)

    nextInterval (alpha, beta) =
      let width = beta - alpha
          width' = 4 * width `div` 3
      in  if side == First
            then (beta, beta + width')
            else (alpha - width', alpha)

    prevInterval (alpha, beta) =
      let width = beta - alpha
          width' = 4 * width `div` 3
      in  if side == Second
            then (beta, beta + width')
            else (alpha - width', alpha)

    widthController prevResult moves dp interval@(alpha,beta) = do
        results <- widthIteration prevResult moves dp interval
        let (good, badMoves) = selectBestEdge interval moves results
            (bestMoves, bestResults) = unzip good
        $info "Score interval: [{} - {}]; number of `too good' moves: {}; number of `too bad' moves: {}" (alpha, beta, length bestMoves, length badMoves)
        if length badMoves == length moves
          then  widthController prevResult bestMoves dp (prevInterval interval)
          else
            case bestResults of
              [] -> return results
              [_] -> return bestResults
              _ -> widthController prevResult bestMoves dp (nextInterval interval)

    widthIteration prevResult moves dp (alpha, beta) = do
      let var = aichData handle
      AICache _ processor _ <- liftIO $ atomically $ readTVar var
      let inputs = [(ai, handle, side, dp, board, move, alpha, beta) | move <- moves]
      results <- process' processor inputs
      joined <- joinResults prevResult results
      return joined

    joinResults :: Maybe AiIterationOutput -> [Either Error (Move, Score)] -> Checkers AiIterationOutput
    joinResults Nothing results =
      case sequence results of
        Right result -> return result
        Left err -> throwError err
    joinResults (Just prevResults) results = zipWithM joinResult prevResults results

    joinResult :: (Move, Score) -> Either Error (Move, Score) -> Checkers (Move, Score)
    joinResult prev@(move, score) (Left TimeExhaused) = do
      $info "Time exhaused while checking move {}, use result from previous depth: {}" (show move, score)
      return prev
    joinResult _ (Left err) = throwError err
    joinResult _ (Right result) = return result

    selectBestEdge (alpha, beta) moves results =
      let (good, bad) = if side == First then (beta, alpha) else (alpha, beta)
          goodResults = [(move, (goodMoves, score)) | (move, (goodMoves, score)) <- zip moves results, score == good]
          badResults = [move | (move, (_, score)) <- zip moves results, score == bad]
      in  (goodResults, badResults)

    select :: AiIterationOutput -> Checkers AiOutput
    select pairs = do
      let best = if side == First then maximum else minimum
          maxScore = best $ map snd pairs
          goodMoves = [move | (move, score) <- pairs, score == maxScore]
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
    evalStateT (cachedScoreAB var params side dp alpha beta board) =<< initState
  where
    initState = do
      now <- liftIO $ getTime Monotonic
      let timeout = case abBaseTime params of
                      Nothing -> Nothing
                      Just sec -> Just $ TimeSpec (fromIntegral sec) 0
      return $ ScoreState rules eval [StackItem Nothing loose] emptyMemo now timeout
    emptyMemo = MovesMemo emptyBoardMap emptyBoardMap

data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssStack :: ! [StackItem]
  , ssMemo :: ! MovesMemo
  , ssStartTime :: TimeSpec
  , ssTimeout :: Maybe TimeSpec
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

-- | Calculate score of the board. 
-- This uses the cache. It is called in the recursive call also.
cachedScoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
              => AICacheHandle rules eval
              -> AlphaBetaParams
              -> Side
              -> DepthParams
              -> Score        -- ^ Alpha
              -> Score        -- ^ Beta
              -> Board
              -> ScoreM rules eval Score
cachedScoreAB var params side dp alpha beta board = do
  let depth = dpCurrent dp
  mbItem <- lift $ lookupAiCache params board dp side var
  case mbItem of
    Just item -> do
      $trace "Cache hit" ()
      let score = cisScore item
      if score < alpha
        then return alpha
        else if score > beta
               then return beta
               else return score

    Nothing -> do
      (score, moves) <- Metrics.timed "ai.score.board" $ scoreAB var params side dp alpha beta board
      when (alpha < score && score < beta) $
          lift $ putAiCache params board dp side score moves var
      return score

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
updateDepth :: (Monad m, HasLogging m, MonadIO m) => Int -> DepthParams -> m DepthParams
updateDepth nMoves dp
  | nMoves <= 4 = do
                let delta = nMoves - 1
                let target = min (dpTarget dp + 1) (dpMax dp - delta)
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
        -> Side
        -> DepthParams
        -> Score        -- ^ Alpha
        -> Score        -- ^ Beta
        -> Board
        -> ScoreM rules eval (Score, [Move])
scoreAB var params side dp alpha beta board
  | isTargetDepth dp = do
      -- target depth is achieved, calculate score of current board directly
      evaluator <- gets ssEvaluator
      let score0 = evalBoard evaluator First side board
      $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      return (score0, [])
  | otherwise = do
      rules <- gets ssRules
      setBest $ if maximize then alpha else beta -- we assume alpha <= beta
      $trace "{}V Side: {}, A = {}, B = {}" (indent, show side, show alpha, show beta)
      moves <- possibleMoves' board

      -- this actually means that corresponding side lost.
      when (null moves) $
        $trace "{}`—No moves left." (Single indent)

      dp' <- updateDepth (length moves) dp
      prevMove <- getPrevMove
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
    
    push :: PossibleMove -> ScoreM rules eval ()
    push move =
      modify $ \st -> st {ssStack = (StackItem (Just move) loose) : (ssStack st)}

    pop = 
      modify $ \st -> st {ssStack = tail (ssStack st)}

    showStack s = unwords $ map show s

    printStack :: ScoreM rules eval ()
    printStack = do
      stack <- gets (reverse . ssStack)
      $trace "{}| side {}: {}" (indent, show side, showStack stack)

    indent = replicate (2*dpCurrent dp) ' '

    getBest =
      gets (siScoreBest . head . ssStack)

    getPrevMove :: ScoreM rules eval (Maybe PossibleMove)
    getPrevMove = 
      gets (siMove . head . ssStack)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      $trace "{}| {} for depth {} : {} => {}" (indent, bestStr, dpCurrent dp, show oldBest, show best)
      modify $ \st -> st {ssStack = update (head $ ssStack st) : tail (ssStack st)}
        where
          update item = item {siScoreBest = best}

    iterateMoves :: [PossibleMove] -> DepthParams -> ScoreM rules eval (Score, [Move])
    iterateMoves [] _ = do
      best <- getBest
      $trace "{}`—All moves considered at this level, return best = {}" (indent, show best)
      return (best, [])
    iterateMoves (move : moves) dp' = do
      timeout <- isTimeExhaused
      when timeout $ do
        -- $info "Timeout exhaused for depth {}." (Single $ dpCurrent dp)
        throwError TimeExhaused
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      evaluator <- gets ssEvaluator
      rules <- gets ssRules
      let board' = applyMoveActions (pmResult move) board
      best <- getBest
      push move 
      -- printStack
      let alpha' = if maximize
                     then max alpha best
                     else alpha
          beta'  = if maximize
                     then beta
                     else min beta best
      score <- cachedScoreAB var params (opposite side) dp' alpha' beta' board'
      $trace "{}| score for side {}: {}" (indent, show side, show score)
      pop
      best <- getBest

      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if (maximize && score >= beta) || (minimize && score <= alpha)
               then do
                    best <- getBest
                    $trace "{}`—Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, show best)
                    return (best, [])
                    
               else iterateMoves moves dp'
        else do
             iterateMoves moves dp'
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks whoMovesNext board =
    evalBoard eval whoAsks whoMovesNext board

