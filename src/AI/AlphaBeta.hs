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
  ( runAI, scoreMove, scoreMoveGroup
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Data.List (sortOn, transpose)
import Data.Text.Format.Heavy
import Data.Aeson
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

import Core.Types
import Core.Board
import Core.BoardMap (labelSetMember)
import Core.Parallel
import Core.Logging
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache

chunksOf :: Int -> [a] -> [[a]]
chunksOf n list
  | length list <= n = [list]
  | otherwise =
      let (first, other) = splitAt n list
      in  first : chunksOf n other

concatE :: [Int] -> [Either e [a]] -> [Either e a]
concatE _ [] = []
concatE (n : ns) (Left e : rest) = replicate n (Left e) ++ concatE ns rest
concatE (n : ns) (Right xs : rest) = map Right xs ++ concatE ns rest

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"
      <*> v .:? "start_depth"
      <*> v .:? "max_combination_depth" .!= 8
      <*> v .:? "dynamic_depth" .!= abDynamicDepth def
      <*> v .:? "deeper_if_bad" .!= False
      <*> v .:? "moves_bound_low" .!= 4
      <*> v .:? "moves_bound_high" .!= 8
      <*> v .:? "time"

instance (GameRules rules, Evaluator eval) => GameAi (AlphaBeta rules eval) where

  type AiStorage (AlphaBeta rules eval) = AICacheHandle rules eval

  createAiStorage ai = do
    cache <- loadAiCache scoreMoveGroup ai
    return cache

  saveAiStorage (AlphaBeta params rules _) cache = do
      -- saveAiCache rules params cache
      return ()

  resetAiStorage ai cache = do
      resetAiCache cache

  chooseMove ai storage gameId side board = do
    (moves, _) <- runAI ai storage gameId side board
    -- liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ calcBoardCounts board
    return moves

  updateAi ai@(AlphaBeta _ rules eval) json =
    case fromJSON json of
      Error e -> error $ "Can't load AI settings: " ++ show e
      Success params -> AlphaBeta params rules (updateEval eval json)

  aiName _ = "default"

instance (GameRules rules, VectorEvaluator eval) => VectorAi (AlphaBeta rules eval) where
  type VectorAiSupport (AlphaBeta rules eval) r = (VectorEvaluatorSupport eval r, rules ~ r)

  aiToVector (AlphaBeta params rules eval) = aiVector V.++ evalVector
    where
      aiVector = V.fromList $ map fromIntegral $ [
                      abDepth params
                    , abCombinationDepth params
                    , abDynamicDepth params
                  ]

      evalVector = evalToVector eval

  aiFromVector rules v = AlphaBeta params rules eval
    where
      params = AlphaBetaParams {
                  abDepth = round (v V.! 0)
                , abStartDepth = Nothing
                , abCombinationDepth = round (v V.! 1)
                , abDynamicDepth = round (v V.! 2)
                , abDeeperIfBad = False
                , abMovesLowBound = abMovesLowBound def
                , abMovesHighBound = abMovesHighBound def
                , abBaseTime = Nothing
              }

      v' = V.drop 3 v

      eval = evalFromVector rules v'

-- | Calculate score of one possible move.
scoreMove :: (GameRules rules, Evaluator eval) => ScoreMoveInput rules eval -> Checkers MoveAndScore
scoreMove (ScoreMoveInput {..}) = do
     let AlphaBeta params rules eval = smiAi
     score <- Monitoring.timed "ai.score.move" $ do
                let board' = applyMoveActions (pmResult smiMove) smiBoard
                score <- doScore rules eval smiCache params smiGameId (opposite smiSide) smiDepth board' smiAlpha smiBeta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show smiMove, dpTarget smiDepth, show e)
                                        throwError e
                                  )
                $info "Check: {} ([{} - {}], depth {}) => {}" (show smiMove, show smiAlpha, show smiBeta, dpTarget smiDepth, show score)
                return score
     
     return $ MoveAndScore smiMove score

scoreMoveGroup :: (GameRules rules, Evaluator eval) => [ScoreMoveInput rules eval] -> Checkers [MoveAndScore]
scoreMoveGroup inputs = go worst [] inputs
  where
    input0 = head inputs
    side = smiSide input0
    alpha = smiAlpha input0
    beta  = smiBeta input0
    maximize = side == First
    minimize = not maximize
    worst = if maximize then alpha else beta

    go _ acc [] = return acc
    go best acc (input : rest) = do
      let input'
            | maximize = input {smiAlpha = prevScore best}
            | otherwise = input {smiBeta = nextScore best}

      result@(MoveAndScore move score) <- scoreMove input'
      let best'
            | maximize && score > best = score
            | minimize && score < best = score
            | otherwise = best

      go best' (acc ++ [result]) rest

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

class Monad m => EvalMoveMonad m where
  checkPrimeVariation :: (GameRules rules, Evaluator eval) => AICacheHandle rules eval -> AlphaBetaParams -> Board -> DepthParams -> m (Maybe PerBoardData)
  getKillerMove :: Int -> m (Maybe MoveAndScore)

instance EvalMoveMonad Checkers where
  checkPrimeVariation var params board dp = do
      lookupAiCache params board dp var

  getKillerMove _ = return Nothing

-- ScoreM instance
instance EvalMoveMonad (StateT (ScoreState rules eval) Checkers) where
  checkPrimeVariation var params board dp = do
      lift $ lookupAiCache params board dp var

  getKillerMove = getGoodMove
  
evalMove :: (EvalMoveMonad m, GameRules rules, Evaluator eval)
        => AlphaBetaParams
        -> AICacheHandle rules eval
        -> Side
        -> DepthParams
        -> Board
        -> Maybe PossibleMove
        -> LabelSet
        -> PossibleMove -> m Int
evalMove params var side dp board mbPrevMove attacked move = do
  prime <- checkPrimeVariation var params board dp
  let victimFields = pmVictims move
      -- nVictims = sum $ map victimWeight victimFields
      promotion = if isPromotion move then 1 else 0
      attackPrevPiece = case mbPrevMove of
                          Nothing -> 0
                          Just prevMove -> if pmEnd prevMove `elem` victimFields
                                             then 5
                                             else 0

      maximize = side == First
      minimize = not maximize

      victimWeight a = case getPiece a board of
                        Nothing -> 0
                        Just (Piece Man _) -> 1
                        Just (Piece King _) -> 3
      
      isAttackPrevPiece = case mbPrevMove of
                            Nothing -> False
                            Just prevMove -> pmEnd prevMove `elem` victimFields

      isAttackKing = any isKing victimFields
      
      isKing a = case getPiece a board of
                   Just (Piece King _) -> True
                   _ -> False

      attackedPiece = let begin = aLabel $ pmBegin move
                      in  if begin `labelSetMember` attacked
                            then getPiece' begin board
                            else Nothing

  case prime of
    Nothing -> if isCapture move
                  then if isAttackPrevPiece
                         then return $ 20 + 3*promotion
                         else if isAttackKing
                                then return $ 10 + 3*promotion
                                else return $ 5*promotion + 3*pmVictimsCount move
                  else case attackedPiece of
                         Nothing -> return promotion
                         Just (Piece King _) -> return 20
                         Just (Piece Man _) -> return 10
    Just primeData -> do
      let score = scoreValue $ itemScore primeData
          signedScore = if maximize then score else -score
      return $ fromIntegral signedScore

sortMoves :: (EvalMoveMonad m, GameRules rules, Evaluator eval)
          => AlphaBetaParams
          -> AICacheHandle rules eval
          -> Side
          -> DepthParams
          -> Board
          -> Maybe PossibleMove
          -> [PossibleMove]
          -> m [PossibleMove]
sortMoves params var side dp board mbPrevMove moves = do
--   if length moves >= 4
--     then do
      let rules = aichRules var
          attacked = boardAttacked side board
      interest <- mapM (evalMove params var side dp board mbPrevMove attacked) moves
      if any (/= 0) interest
        then return $ map fst $ sortOn (negate . snd) $ zip moves interest
        else return moves
--     else return moves

rememberGoodMove :: Int -> Side -> PossibleMove -> Score -> ScoreM rules eval ()
rememberGoodMove depth side move score = do
  goodMoves <- gets ssBestMoves
  let goodMoves' = case M.lookup depth goodMoves of
                     Nothing -> M.insert depth (MoveAndScore move score) goodMoves
                     Just (MoveAndScore _ prevScore)
                      | (maximize && score > prevScore) || (minimize && score < prevScore)
                          -> M.insert depth (MoveAndScore move score) goodMoves
                      | otherwise -> goodMoves
      maximize = side == First
      minimize = not maximize
  modify $ \st -> st {ssBestMoves = goodMoves'}

getGoodMove :: Int -> ScoreM rules eval (Maybe MoveAndScore)
getGoodMove depth = do
  goodMoves <- gets ssBestMoves
  return $ M.lookup depth goodMoves


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
    options <- depthDriver =<< getPossibleMoves handle side board
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

    worseThan s1 s2
      | maximize = s1 < s2
      | otherwise = s1 > s2

--     preselect = do
--       moves <- getPossibleMoves handle side board
--       let simple = DepthParams {
--                     dpInitialTarget = 2
--                   , dpTarget = 2
--                   , dpCurrent = -1
--                   , dpMax = 4
--                   , dpMin = 2
--                   , dpForcedMode = False
--                   , dpStaticMode = False
--                 }
--       result <- sortMoves params handle side simple board Nothing moves
--       $debug "Pre-selected options: {}" (Single $ show result)
--       return result

    preselect :: Depth -> [PossibleMove] -> Checkers [Score]
    preselect depth moves = do
      let simple = DepthParams {
                    dpInitialTarget = depth
                  , dpTarget = depth
                  , dpCurrent = -1
                  , dpMax = 6
                  , dpMin = depth
                  , dpForcedMode = False
                  , dpStaticMode = False
                  , dpReductedMode = False
                }
      $info "Preselecting; number of possible moves = {}, depth = {}" (length moves, dpTarget simple)
      options <- scoreMoves' False moves simple (loose, win)
      let key = if maximize
                  then negate . rScore
                  else rScore
      return $ map key options
--       let sorted = sortOn key options
--           bestOptions = take (abMovesHighBound params) sorted
--       let result = map fst sorted
--       $debug "Pre-selected options: {}" (Single $ show result)
--       return result

    depthStep :: Depth
    depthStep = 5

    depthDriver :: [PossibleMove] -> Checkers DepthIterationOutput
    depthDriver moves =
      case abBaseTime params of
          Nothing -> do
            let target = abDepth params
                preselectDepth =
                  if target <= depthStep
                    then target
                    else let m = target `mod` depthStep
                         in  head $ filter (>= 2) [m + depthStep, m + depthStep + depthStep .. target]
                startDepth = case abStartDepth params of
                               Nothing -> Nothing
                               Just start -> Just $ max 2 $ preselectDepth + start - target
                input = DepthIterationInput {
                          diiParams = params {abDepth = preselectDepth, abStartDepth = startDepth},
                          diiMoves = moves,
                          diiPrevResult = Nothing,
                          diiSortKeys = Nothing
                        }
            goIterative target input
          Just time -> do
            let input =  DepthIterationInput {
                           diiParams = params,
                           diiMoves = moves,
                           diiPrevResult = Nothing,
                           diiSortKeys = Nothing
                        }
            repeatTimed' "runAI" time goTimed input

  
    goTimed :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    goTimed input = do
      ret <- tryC $ go input
      case ret of
        Right result -> return result
        Left TimeExhaused ->
          case diiPrevResult input of
            Just result -> return (result, Nothing)
            Nothing -> return ([MoveAndScore move 0 | move <- diiMoves input], Nothing)
        Left err -> throwError err

    goIterative :: Depth -> DepthIterationInput -> Checkers DepthIterationOutput
    goIterative target input = do
      (output, mbNextInput) <- go input
      case mbNextInput of
        Nothing -> do
          let bad (MoveAndScore _ score) = (maximize && score <= score0 - 1) || (minimize && score >= score0 + 1)
          if abDeeperIfBad params && all bad output
            then do
                 let nextInput = deeper (abDepth $ diiParams input) 1 output input
                 $info "All moves seem bad, re-think one step further" ()
                 (output', _) <- go nextInput
                 return output'
            else return output
        Just nextInput ->
          if abDepth (diiParams nextInput) <= target
            then goIterative target nextInput
            else return output

    go :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    go input@(DepthIterationInput {..}) = do
      let depth = abDepth diiParams
      if length diiMoves <= 1 -- Just one move possible
        then do
          $info "There is only one move possible; just do it." ()
          return ([MoveAndScore move score0 | move <- diiMoves], Nothing)
                                                             
        else do
          let var = aichData handle
          $info "Selecting a move. Side = {}, depth = {}, number of possible moves = {}" (show side, depth, length diiMoves)
          dp <- updateDepth params diiMoves $ DepthParams {
                     dpInitialTarget = depth
                   , dpTarget = depth
                   , dpCurrent = -1
                   , dpMax = abCombinationDepth diiParams + depth
                   , dpMin = min depth $ fromMaybe depth (abStartDepth diiParams)
                   , dpStaticMode = False
                   , dpForcedMode = False
                   , dpReductedMode = False
                   }
          let needDeeper = abDeeperIfBad params && score0 `worseThan` 0
          let dp'
                | needDeeper = dp {
                                    dpTarget = min (dpMax dp) (dpTarget dp + 1)
                                  }
                | otherwise = dp

          sortedMoves <-
              case diiSortKeys of
                Nothing -> return diiMoves
                Just keys -> do
                  let moves' = map snd $ sortOn fst $ zip keys diiMoves
                  $debug "Sort moves: {} => {}" (show $ zip keys diiMoves, show moves')
                  return moves'

          result <- widthController True True diiPrevResult sortedMoves dp' =<< mkInitInterval depth (isQuiescene diiMoves)
          $debug "Depth iteration result: {}" (Single $ show result)
          -- In some corner cases, there might be 1 or 2 possible moves,
          -- so the timeout would allow us to calculate with very big depth;
          -- too big depth does not decide anything in such situations.
          if depth < 50
            then do
              let input' = deeper depth depthStep result input
              return (result, Just input')
            else return (result, Nothing)

    deeper :: Depth -> Depth -> DepthIterationOutput -> DepthIterationInput -> DepthIterationInput
    deeper depth step prevOutput input =
      let start' = fmap (+step) (abStartDepth params)
          params' = params {abDepth = depth + step, abStartDepth = start'}
          keys = map rScore prevOutput
          moves' = map rMove prevOutput
          signedKeys = if maximize then map negate keys else keys
      in  input {
                  diiParams = params',
                  diiPrevResult = Just prevOutput,
                  diiMoves = moves',
                  diiSortKeys = Just signedKeys
                }

    score0 = evalBoard eval First board

    -- | Initial (alpha, beta) interval
    mkInitInterval :: Depth -> Bool -> Checkers (Score, Score)
    mkInitInterval depth quiescene = do
      let delta
            | depth < abDepth params = fromIntegral $ max 1 $ abDepth params - depth
            | not quiescene = Score 1 500
            | otherwise = Score 0 600
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

    scale :: ScoreBase -> Score -> Score
    scale k s
      | sNumeric s < 1 = Score 1 (sPositional s)
      | otherwise = k `scaleScore` s

    nextInterval :: (Score, Score) -> (Score, Score)
    nextInterval (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scale` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if maximize
            then (alpha, max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), beta)

    prevInterval :: (Score, Score) -> (Score, Score)
    prevInterval (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scale` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if minimize
            then (alpha, max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), beta)

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
          return [MoveAndScore move alpha | move <- moves]
        else do
            results <- widthIteration prevResult moves dp interval
            let (bestResults, badScore, badMoves) = selectBestEdge interval moves results
                bestMoves = map rMove bestResults
            if length badMoves == length moves
              then
                if allowPrev
                  then
                    if (maximize && alpha <= loose) || (minimize && beta >= win)
                      then do
                        $info "All moves are `too bad'; but there is no worse interval, return all what we have" ()
                        return [MoveAndScore move badScore | move <- moves]
                      else do
                        let interval' = prevInterval interval
                        $info "All moves are `too bad'; consider worse scores interval: [{} - {}]" interval'
                        widthController False True prevResult badMoves dp interval'
                  else do
                    $info "All moves are `too bad' ({}), but we have already checked worse interval; so this is the real score." (Single badScore)
                    return [MoveAndScore move badScore | move <- moves]
              else
                case bestResults of
                  [] -> return results
                  [_] -> do
                    $info "Exactly one move is `too good'; do that move." ()
                    return bestResults
                  _ ->
                    if allowNext
                      then
                        if (maximize && beta >= win) || (minimize && alpha <= loose)
                          then do
                            $info "Some moves ({} of them) are `too good'; but there is no better interval, return all of them." (Single $ length bestMoves)
                            return bestResults
                          else do
                            let interval'@(alpha',beta') = nextInterval interval
                            $info "Some moves ({} of them) are `too good'; consider better scores interval: [{} - {}]" (length bestMoves, alpha', beta')
                            widthController True False prevResult bestMoves dp interval'
                          else do
                            $info  "Some moves ({} of them) are `too good'; but we have already checked better interval; so this is the real score" (Single $ length bestMoves)
                            return bestResults

    getJobIndicies :: Int -> Checkers [Int]
    getJobIndicies count = liftIO $ atomically $ do
        lastIndex <- readTVar (aichJobIndex handle)
        let nextIndex = lastIndex + count
        writeTVar (aichJobIndex handle) nextIndex
        return [lastIndex+1 .. nextIndex]

    scoreMoves :: Bool -> [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers [Either Error MoveAndScore]
    scoreMoves byOne moves dp (alpha, beta) = do
      nThreads <- asks (aiThreads . gcAiConfig . csConfig)
      let var = aichData handle
      let processor = aichProcessor handle
          n = length moves
      indicies <- getJobIndicies n
      let inputs = [
            ScoreMoveInput {
              smiAi = ai,
              smiCache = handle,
              smiGameId = gameId,
              smiSide = side,
              smiIndex = index,
              smiDepth = dp,
              smiBoard = board,
              smiMove = move,
              smiAlpha = alpha,
              smiBeta = beta
            } | (move, index) <- zip moves indicies ]

          groups
            | byOne = [[input] | input <- inputs]
            | otherwise = transpose $ chunksOf nThreads inputs

      results <- process' processor groups
      return $ concatE (map length groups) results

    scoreMoves' :: Bool -> [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    scoreMoves' byOne moves dp (alpha, beta) = do
      results <- scoreMoves byOne moves dp (alpha, beta)
      case sequence results of
        Right result -> return result
        Left err -> throwError err

    widthIteration :: Maybe DepthIterationOutput -> [PossibleMove] -> DepthParams -> (Score, Score) -> Checkers DepthIterationOutput
    widthIteration prevResult moves dp (alpha, beta) = do
      $info "`- Considering scores interval: [{} - {}], depth = {}, number of moves = {}" (alpha, beta, dpTarget dp, length moves)
      results <- scoreMoves False moves dp (alpha, beta)
      joinResults prevResult results

    joinResults :: Maybe DepthIterationOutput -> [Either Error MoveAndScore] -> Checkers DepthIterationOutput
    joinResults Nothing results =
      case sequence results of
        Right result -> return result
        Left err -> throwError err
    joinResults (Just prevResults) results = zipWithM joinResult prevResults results

    joinResult :: MoveAndScore -> Either Error MoveAndScore -> Checkers MoveAndScore
    joinResult prev@(MoveAndScore move score) (Left TimeExhaused) = do
      $info "Time exhaused while checking move {}, use result from previous depth: {}" (show move, score)
      return prev
    joinResult _ (Left err) = throwError err
    joinResult _ (Right result) = return result

    selectBestEdge :: (Score, Score) -> [PossibleMove] -> [MoveAndScore] ->
                      ([MoveAndScore], Score, [PossibleMove])
    selectBestEdge (alpha, beta) moves results =
      let (good, bad) = if maximize then (beta, alpha) else (alpha, beta)
          goodResults = [result | result <- results, not (rScore result `worseThan` good)]
          badResults = [rMove result | result <- results, not (rScore result `betterThan` bad)]
      in  (goodResults, bad, badResults)

    select :: DepthIterationOutput -> Checkers AiOutput
    select pairs = do
      let best = if maximize then maximum else minimum
          maxScore = best $ map rScore pairs
          goodMoves = [rMove result | result <- pairs, rScore result == maxScore]
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
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers Score
doScore rules eval var params gameId side dp board alpha beta = do
    initState <- mkInitState
    out <- evalStateT (cachedScoreAB var params input) initState
    return $ soScore out
  where
    input = ScoreInput side dp alpha beta board Nothing 
    mkInitState = do
      now <- liftIO $ getTime RealtimeCoarse
      let timeout = case abBaseTime params of
                      Nothing -> Nothing
                      Just sec -> Just $ TimeSpec (fromIntegral sec) 0
      return $ ScoreState rules eval gameId [loose] M.empty now timeout

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
  mbItem <- lift $ lookupAiCache params board dp var
  mbCached <- case mbItem of
                Just item -> do
                  let cachedScore = itemScore item
                  -- it is possible that this value was put to cache with different
                  -- values of alpha/beta; but we have to maintain the property of
                  -- AB-section: alpha <= result <= beta. So here we clamp the value
                  -- that we got from cache.
                  case itemBound item of
                    Exact -> return $ Just $ ScoreOutput (clamp alpha beta cachedScore) False
                    Alpha -> if cachedScore <= alpha
                               then return $ Just $ ScoreOutput alpha False
                               else return Nothing
                    Beta  -> if cachedScore >= beta
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
          item = PerBoardData (dpLast dp) score bound
          item' = PerBoardData (dpLast dp) (negate score) bound
      when (bound == Exact && soQuiescene out && not (dpStaticMode dp)) $ do
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
    | deepen = do
                  let delta = fromIntegral nMoves - 1
                  let target = min (dpTarget dp + 1) (dpMax dp - delta)
                  let indent = replicate (fromIntegral $ 2*dpCurrent dp) ' '
                  let static = dpCurrent dp > dpInitialTarget dp + abDynamicDepth params
                  $verbose "{}| there is only one move, increase target depth to {}"
                          (indent, target)
                  return $ dp {
                            dpCurrent = dpCurrent dp + 1,
                            dpTarget = target,
                            dpForcedMode = forced || dpForcedMode dp,
                            dpStaticMode = static
                          }
    | nMoves > abMovesHighBound params && canRazor = do
                  let target = max (dpCurrent dp + 1) (dpInitialTarget dp)
                  let indent = replicate (fromIntegral $ 2*dpCurrent dp) ' '
                  $verbose "{}| there are too many moves, decrease target depth to {}"
                          (indent, target)
                  return $ dp {dpCurrent = dpCurrent dp + 1, dpTarget = target, dpReductedMode = True}
    | otherwise = return $ dp {dpCurrent = dpCurrent dp + 1}
  where
    nMoves = length moves
    forced = any isCapture moves || any isPromotion moves
    deepen = if dpCurrent dp <= dpInitialTarget dp
               then nMoves <= abMovesLowBound params
               else forced

    canRazor = isQuiescene moves &&
               dpForcedMode dp &&
               not (dpReductedMode dp)

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
      now <- liftIO $ getTime RealtimeCoarse
      return $ start + delta <= now

-- | Calculate score for the board.
-- This implements the alpha-beta section algorithm itself.
scoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
        => AICacheHandle rules eval
        -> AlphaBetaParams
        -> ScoreInput
        -> ScoreM rules eval ScoreOutput
scoreAB var params input
  | alpha == beta = do
      $verbose "Alpha == Beta == {}, return it" (Single $ show alpha)
      quiescene <- checkQuiescene
      return $ ScoreOutput alpha quiescene

  | isTargetDepth dp = do
      -- target depth is achieved, calculate score of current board directly
      evaluator <- gets ssEvaluator
      let score0 = evalBoard' evaluator board
      $verbose "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      quiescene <- checkQuiescene
      return $ ScoreOutput score0 quiescene

  | otherwise = do
      evaluator <- gets ssEvaluator
      let score0 = evalBoard' evaluator board
      futilePrunned <- checkFutility
      case futilePrunned of
        Just out@(ScoreOutput score0 quiescene) -> do
            $verbose "Further search is futile, return current score0 = {}" (Single $ show score0)
            return out
        Nothing -> do
                
          moves <- lift $ getPossibleMoves var side board
          let quiescene = isQuiescene moves
          let worst
                | maximize = alpha
                | otherwise = beta

          if null moves
            -- this actually means that corresponding side lost.
            then do
              $verbose "{}`—No moves left." (Single indent)
              return $ ScoreOutput worst True
            else
              if dpStaticMode dp && isQuiescene moves
                -- In static mode, we are considering forced moves only.
                -- If we have reached a quiescene, then that's all.
                then do
                  $verbose "Reached quiescene in static mode; return current score0 = {}" (Single $ show score0)
                  return $ ScoreOutput score0 True
                else do
                  -- first, let "best" be the worse possible value
                  let best
                        | dpStaticMode dp = evalBoard' evaluator board
                        | otherwise = worst

                  push best
                  $verbose "{}V Side: {}, A = {}, B = {}" (indent, show side, show alpha, show beta)
                  rules <- gets ssRules
                  dp' <- updateDepth params moves dp
                  let prevMove = siPrevMove input
                  moves' <- sortMoves params var side dp board prevMove moves
--                   let depths = correspondingDepths (length moves') score0 quiescene dp'
                  let depths = repeat dp'
                  out <- iterateMoves $ zip3 [1..] moves' depths
                  pop
                  return out

  where

    side = siSide input
    dp = siDepth input
    alpha = siAlpha input
    beta = siBeta input
    board = siBoard input

    canReduceDepth :: Score -> Bool -> Bool
    canReduceDepth score0 quiescene =
      not (dpForcedMode dp) &&
      not (dpReductedMode dp) &&
               dpCurrent dp >= 4 &&
               quiescene &&
               score0 > alpha &&
               score0 < beta &&
               score0 > -10 &&
               score0 < 10 

    correspondingDepths :: Int -> Score -> Bool -> DepthParams -> [DepthParams]
    correspondingDepths nMoves score0 quiescene depth =
      if (nMoves <= abMovesHighBound params) || not (canReduceDepth score0 quiescene)
        then replicate nMoves depth
        else let reducedDepth = depth {
                                  dpTarget = min (dpMin dp) (dpTarget depth),
                                  dpReductedMode = True
                                }
             in  replicate (abMovesHighBound params) depth ++ repeat reducedDepth

    checkFutility :: ScoreM rules eval (Maybe ScoreOutput)
    checkFutility = do
      evaluator <- gets ssEvaluator
      quiescene <- checkQuiescene
      let score0 = evalBoard' evaluator board
          best = if maximize then alpha else beta
          isBad = if maximize
                    then score0 <= alpha + 1
                    else score0 >= beta - 1

      if (dpCurrent dp >= dpTarget dp - 1) &&
          not (dpForcedMode dp) &&
          quiescene &&
          score0 >= -10 &&
          score0 <= 10 &&
          isBad
        then return $ Just $ ScoreOutput score0 quiescene
        else return Nothing

    evalBoard' :: eval -> Board -> Score
    evalBoard' evaluator board = result
      where
        score = evalBoard evaluator First board
        result
          | maximize && sNumeric score == sNumeric win   = score - Score (fromIntegral $ dpCurrent dp) 0
          | minimize && sNumeric score == sNumeric loose = score + Score (fromIntegral $ dpCurrent dp) 0
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
    
    indent = replicate (fromIntegral $ 2*dpCurrent dp) ' '

    getBest =
      gets (head . ssBestScores)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      $verbose "{}| {} for depth {} : {} => {}" (indent, bestStr, dpCurrent dp, show oldBest, show best)
      modify $ \st -> st {ssBestScores = best : tail (ssBestScores st)}

    opponentMoves :: ScoreM rules eval [PossibleMove]
    opponentMoves = do
      rules <- gets ssRules
      lift $ getPossibleMoves var (opposite side) board

    isInteresting move = do
      opMoves <- opponentMoves
      let victims = concatMap pmVictims opMoves
      return $ {- pmBegin move `elem` victims || -} length (pmVictims move) >= 2 || isPromotion move

    mkIntervals zero (alpha, beta)
      | maximize =
        let mid = min (alpha + zero) beta
        in  [(alpha, mid), (alpha, beta)]
      | otherwise =
        let mid = max (beta - zero) alpha
        in  [(mid, beta), (alpha, beta)]

    checkMove :: AICacheHandle rules eval -> AlphaBetaParams -> ScoreInput -> Int -> ScoreM rules eval ScoreOutput
    checkMove var params input i = do
        let alpha = siAlpha input
            beta  = siBeta input
            width = beta - alpha
            zeroWidth = Score 0 300
        intervals <- do
              let interesting = i <= 1
              if interesting || width <= zeroWidth
                then return [(alpha, beta)]
                else return $ mkIntervals zeroWidth (alpha, beta)
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


    iterateMoves :: [(Int,PossibleMove, DepthParams)] -> ScoreM rules eval ScoreOutput
    iterateMoves [] = do
      best <- getBest
      $verbose "{}`—All moves considered at this level, return best = {}" (indent, show best)
      quiescene <- checkQuiescene
      return $ ScoreOutput best quiescene
    iterateMoves ((i,move, dp) : moves) = do
      timeout <- isTimeExhaused
      when timeout $ do
        -- $info "Timeout exhaused for depth {}." (Single $ dpCurrent dp)
        throwError TimeExhaused
      $verbose "{}|+Check move of side {}: {}" (indent, show side, show move)
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
                    , siBoard = markAttacked rules $ applyMoveActions (pmResult move) board
                    , siDepth = dp
                  }
      out <- cachedScoreAB var params input'
      let score = soScore out
      $verbose "{}| score for side {}: {}" (indent, show side, show score)

      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if (maximize && score >= beta) || (minimize && score <= alpha)
               then do
                    -- rememberGoodMove (dpCurrent dp) side move score
                    Monitoring.distribution "ai.section.at" $ fromIntegral i
                    $verbose "{}`—Return {} for depth {} = {}" (indent, bestStr, dpCurrent dp, show score)
                    quiescene <- checkQuiescene
                    return $ ScoreOutput score quiescene
                    
               else iterateMoves moves
        else do
             iterateMoves moves
        
instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks board =
    evalBoard eval whoAsks board

