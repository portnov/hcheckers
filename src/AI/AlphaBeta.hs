{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

{-
 - This module contains an implementation of alpha-beta-pruning algorithm
 - with small improvements.
 -}

module AI.AlphaBeta
  ( runAI, scoreMove, scoreMoveGroup, mkDepthParams
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Maybe
import Data.Default
import Data.List (sortOn)
import Data.Text.Format.Heavy
import Data.Aeson
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Game
import Core.Parallel
import Core.Logging
import qualified Core.Monitoring as Monitoring
import AI.Session
import AI.AlphaBeta.Types
import AI.AlphaBeta.Instances () -- import instances only
import AI.AlphaBeta.Cache
import AI.AlphaBeta.Persistent

concatE :: [Int] -> [Either e [a]] -> [Either e a]
concatE _ [] = []
concatE (n : ns) (Left e : rest) = replicate n (Left e) ++ concatE ns rest
concatE (n : ns) (Right xs : rest) = map Right xs ++ concatE ns rest

mkDepthParams :: Depth -> AlphaBetaParams -> DepthParams
mkDepthParams depth params =
  DepthParams {
     dpInitialTarget = depth
   , dpTarget = depth
   , dpCurrent = -1
   , dpMax = abCombinationDepth params + depth
   , dpMin = min depth $ fromMaybe depth (abStartDepth params)
   , dpStaticMode = False
   , dpForcedMode = False
   , dpReductedMode = False
   }

getBgSessionId :: (GameRules rules, VectorEvaluator eval)
               => AICacheHandle rules eval
               -> GameId
               -> Checkers (Maybe AiSessionId)
getBgSessionId handle gameId = do
  bgSessions <- liftIO $ readTVarIO (aichBackgroundSession handle)
  return $ M.lookup gameId bgSessions

stopBgSession :: (GameRules rules, VectorEvaluator eval)
               => AICacheHandle rules eval
               -> GameId
               -> Checkers ()
stopBgSession handle gameId = do
  mbBgSessionId <- getBgSessionId handle gameId
  case mbBgSessionId of
    Nothing -> return ()
    Just bgSessionId -> do
      $info "Send a signal to stop background session" ()
      signalStopAiSession bgSessionId

newBgSession :: (GameRules rules, VectorEvaluator eval)
               => AICacheHandle rules eval
               -> GameId
               -> Checkers (AiSessionId, AiSession)
newBgSession handle gameId = do
  (bgSessionId, bgSession) <- newAiSession
  liftIO $ atomically $ modifyTVar (aichBackgroundSession handle) $ \sessions ->
      M.insert gameId bgSessionId sessions
  return (bgSessionId, bgSession)

instance (GameRules rules, VectorEvaluator eval, ToJSON eval) => GameAi (AlphaBeta rules eval) where

  type AiStorage (AlphaBeta rules eval) = AICacheHandle rules eval

  createAiStorage ai@(AlphaBeta params rules eval) = do
    cache <- loadAiCache scoreMoveGroup rules
    return cache

  saveAiStorage (AlphaBeta params rules _) handle = do
      saveAiData rules (aichData handle)
      return ()

  resetAiStorage ai cache = do
      resetAiCache cache

  chooseMove ai storage gameId side aiSession board = Monitoring.timed "ai.choose.move" $ do
    stopBgSession storage gameId
    (moves, _) <- runAI ai storage gameId side aiSession board
    -- liftIO $ atomically $ writeTVar (aichCurrentCounts storage) $ calcBoardCounts board
    Monitoring.distribution "ai.chosen.moves.count" $ fromIntegral $ length moves
    when (length moves > 1) $
      Monitoring.increment "ai.ambigous.choice"
    return moves

  afterMoveSelected ai@(AlphaBeta params rules eval) storage gameId side board pm = do
    when (abThinkInBackground params) $ Monitoring.timed "ai.background" $ do
      let side' = opposite side
      (bgSessionId, bgSession) <- newBgSession storage gameId
      forkCheckers $ do
        let params' = params {abDepth = abDepth params + 1, abDeepeningWithinTimeout = False}
            ai' = AlphaBeta params' rules eval
            moves' = possibleMoves rules side' board
        case moves' of
          [] -> $debug "Opposite side does not have moves, do not think for it" ()
          [move] -> do
            let board' = applyMoveActions (pmResult move) board
            $info "Opposite side has only one possible move {}. Start thinking in backgrond for side {} after that move" (show move, show side)
            (moves, _) <- runAI ai' storage gameId side bgSession board'
            $info "AI will select next moves for side {}: {}" (show side, show moves)
          _ -> do
            $info "Start thinking in background for side {}" (Single $ show side')
            (moves, _) <- runAI ai' storage gameId side' bgSession board
            $info "In place of side {}, AI would select moves: {}" (show side', show moves)
      return ()

  decideDrawRequest ai@(AlphaBeta params rules eval) storage gameId side aiSession board = do
    case abDrawPolicy params of
      AlwaysAccept -> return True
      AlwaysDecline -> return False
      AcceptIfLosing -> do
        let depth = mkDepthParams (abDepth params) params
        scoreOut <- doScore rules eval storage params gameId side aiSession depth board loose win
        let loosing = (side == First && soScore scoreOut < 0) || (side == Second && soScore scoreOut > 0)
        if loosing
            then return True
            else return False

  updateAi ai@(AlphaBeta _ rules eval) json =
    case fromJSON json of
      Error e -> error $ "Can't load AI settings: " ++ show e
      Success params -> AlphaBeta params rules (updateEval eval json)

  aiName _ = "default"

instance (GameRules rules, VectorEvaluator eval, ToJSON eval) => VectorAi (AlphaBeta rules eval) where
  type VectorAiSupport (AlphaBeta rules eval) r = (rules ~ r)

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
                , abDepthStep = abDepthStep def
                , abDeeperIfBad = False
                , abDeeperIfAmbigous = Nothing
                , abMovesLowBound = abMovesLowBound def
                , abMovesHighBound = abMovesHighBound def
                , abInitWindowWidth = abInitWindowWidth def
                , abTimeout = abTimeout def
                , abDeepeningWithinTimeout = False
                , abRandomOpeningDepth = 1
                , abRandomOpeningOptions = 1
                , abDrawPolicy = AlwaysAccept
                , abThinkInBackground = False
              }

      v' = V.drop 3 v

      eval = evalFromVector rules v'

-- | Calculate score of one possible move.
scoreMove :: (GameRules rules, VectorEvaluator eval) => ScoreMoveInput rules eval -> Checkers MoveAndScore
scoreMove (ScoreMoveInput {..}) = do
     let AlphaBeta params rules eval = smiAi
     out <- Monitoring.timed "ai.score.move" $ do
                let board' = applyMoveActions (pmResult smiMove) smiBoard
                out <- doScore rules eval smiCache params smiGameId (opposite smiSide) smiAiSession smiDepth board' smiAlpha smiBeta
                          `catchError` (\(e :: Error) -> do
                                        $info "doScore: move {}, depth {}: {}" (show smiMove, dpTarget smiDepth, show e)
                                        throwError e
                                  )
                let interrupt
                      | soInterrupted out = "[interrupted]" :: String
                      | otherwise = ""
                $info "Check: {} ([{} - {}], depth {}) => {}{}" (show smiMove, show smiAlpha, show smiBeta, dpTarget smiDepth, show (soScore out), interrupt)
                return out

     return $ MoveAndScore smiMove (soScore out) (soInterrupted out)

scoreMoveGroup :: (GameRules rules, VectorEvaluator eval) => [ScoreMoveInput rules eval] -> Checkers [MoveAndScore]
scoreMoveGroup inputs = go [] inputs
  where
    input0 = head inputs
    side = smiSide input0
    alpha = smiAlpha input0
    beta  = smiBeta input0
    bestVar = smiBest input0
    maximize = side == First
    minimize = not maximize

    go acc [] = return acc
    go acc (input : rest) = do
      best <- liftIO $ readTVarIO bestVar
      let input'
            | maximize = input {smiAlpha = prevScore best}
            | otherwise = input {smiBeta = nextScore best}

      -- FIXME: check for interruption
      result@(MoveAndScore move score interrupted) <- scoreMove input'
      unless interrupted $
        liftIO $ atomically $ do
          updatedBest <- readTVar bestVar
          let best'
                | maximize && score > updatedBest = score
                | minimize && score < updatedBest = score
                | otherwise = updatedBest
          writeTVar bestVar best'

      go (acc ++ [result]) rest

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
    let moves = possibleMoves rules side board
    Monitoring.distribution "ai.possible_moves.count" (fromIntegral $ length moves)
    return moves
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
  checkPrimeVariation :: (GameRules rules) => AICacheHandle rules eval -> CacheKey -> m (Maybe PerBoardData)
  getKillerMove :: Int -> m (Maybe MoveAndScore)

instance EvalMoveMonad Checkers where
  checkPrimeVariation var key = do
    lookupAiCacheC key var

  getKillerMove _ = return Nothing

-- ScoreM instance
instance EvalMoveMonad (StateT (ScoreState rules eval) Checkers) where
  checkPrimeVariation var key@(eval,board) = do
      cache <- gets ssCache
      lookupAiCacheS' key cache

  getKillerMove = getGoodMove

evalMove :: (EvalMoveMonad m, GameRules rules, VectorEvaluator eval)
        => AICacheHandle rules eval
        -> eval
        -> Side
        -> Board
        -> Maybe PossibleMove
        -> LabelSet
        -> PossibleMove
        -> m Int
evalMove var eval side board mbPrevMove attacked move = do
  let cacheKey = mkCacheKey eval $ applyMoveActions (pmResult move) board
  prime <- checkPrimeVariation var cacheKey
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

sortMoves :: (EvalMoveMonad m, GameRules rules, VectorEvaluator eval)
          => eval
          -> AlphaBetaParams
          -> AICacheHandle rules eval
          -> Side
          -> Board
          -> Maybe PossibleMove
          -> [PossibleMove]
          -> m [PossibleMove]
sortMoves eval params var side board mbPrevMove moves = do
--   if length moves >= 4
--     then do
      let rules = aichRules var
          attacked = boardAttacked side board
      interest <- mapM (evalMove var eval side board mbPrevMove attacked) moves
      if any (/= 0) interest
        then return $ map fst $ sortOn (negate . snd) $ zip moves interest
        else return moves
--     else return moves

rememberGoodMove :: Int -> Side -> PossibleMove -> Score -> ScoreM rules eval ()
rememberGoodMove depth side move score = do
  goodMoves <- gets ssBestMoves
  let goodMoves' = case M.lookup depth goodMoves of
                     Nothing -> M.insert depth (MoveAndScore move score False) goodMoves
                     Just (MoveAndScore _ prevScore _)
                      | (maximize && score > prevScore) || (minimize && score < prevScore)
                          -> M.insert depth (MoveAndScore move score False) goodMoves
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
runAI :: (GameRules rules, VectorEvaluator eval)
      => AlphaBeta rules eval
      -> AICacheHandle rules eval
      -> GameId
      -> Side
      -> AiSession
      -> Board
      -> Checkers AiOutput
runAI ai@(AlphaBeta params rules eval) handle gameId side aiSession board = do
    timeout <- getTimeout params
    forkCheckers $ timeoutSignaller (aiStopSignal aiSession) timeout
    options <- depthDriver =<< getPossibleMoves handle side board
    if null options
      then return ([], if maximize then loose else win)
      else do
        output <- select options
        let bestScore = sNumeric $ snd output
        let shift = bestScore - sNumeric score0
        unless (any rInterrupted options) $
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

    depthDriver :: [PossibleMove] -> Checkers DepthIterationOutput
    depthDriver moves = do
      if abDeepeningWithinTimeout params
        then do
          let input =  DepthIterationInput {
                         diiParams = params,
                         diiMoves = moves,
                         diiPrevResult = Nothing,
                         diiSortKeys = Nothing,
                         diiAmbigousMovesScore = Nothing
                      }
          timeout <- getTimeout params
          repeatTimed' "runAI" timeout timedDepthDriver input
        else do
          nThreads <- asks (aiThreads . gcAiConfig . csConfig)
          let target = abDepth params
              depthStep = abDepthStep params
              preselectDepth =
                if target <= depthStep -- || length moves < nThreads
                  then target
                  else let m = target `mod` depthStep
                       in  head $ filter (>= 2) [m, m + depthStep .. target]
              startDepth = case abStartDepth params of
                             Nothing -> Nothing
                             Just start -> Just $ max 2 $ preselectDepth + start - target
              input = DepthIterationInput {
                        diiParams = params {
                                      abDepth = preselectDepth,
                                      abStartDepth = startDepth,
                                      abDynamicDepth = min preselectDepth (preselectDepth - abDepth params + abDynamicDepth params)
                                    },
                        diiMoves = moves,
                        diiPrevResult = Nothing,
                        diiSortKeys = Nothing,
                        diiAmbigousMovesScore = Nothing
                      }
          iterativeDepthDriver target (abDeeperIfAmbigous params) input

    timedDepthDriver :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    timedDepthDriver input = do
      ret@(output,_) <- doDepthIteration input
      if any rInterrupted output
        then
          case diiPrevResult input of
            Just prevResult -> return (prevResult, Nothing)
            Nothing -> return ([MoveAndScore move 0 True | move <- diiMoves input], Nothing)
        else return ret

    iterativeDepthDriver :: Depth -> Maybe Depth -> DepthIterationInput -> Checkers DepthIterationOutput
    iterativeDepthDriver target mbAmbigousTarget input = do
      (output, mbNextInput) <- doDepthIteration input
      case mbNextInput of
        Nothing -> do
          let bad (MoveAndScore _ score _) = (maximize && score <= score0 - 1 && score > loose) || (minimize && score >= score0 + 1 && score < win)
          if abDeeperIfBad params && all bad output
            then do
                 let nextInput = deeper (abDepth $ diiParams input) 1 output input
                 $info "All moves seem bad, re-think one step further" ()
                 (output', _) <- doDepthIteration nextInput
                 return output'
            else return output
        Just nextInput -> do
          if abDepth (diiParams nextInput) <= target
            then iterativeDepthDriver target mbAmbigousTarget nextInput
            else do
              case mbAmbigousTarget of
                Nothing -> return output
                Just maxDepth -> do
                  let best = if maximize then maximum else minimum
                      maxScore = best $ map rScore output
                      goodMoves = [rMove result | result <- output, rScore result == maxScore]
                      nGoodMoves = length goodMoves
                      params' = (diiParams nextInput) {abDepth = maxDepth, abDepthStep = 1}
                      nextInput' = nextInput {
                                     diiMoves = goodMoves,
                                     diiSortKeys = Nothing,
                                     diiParams = params',
                                     diiAmbigousMovesScore = Just maxScore
                                   }
                      isCritical = (maxScore == win) || (maxScore == loose)
                  if not isCritical && nGoodMoves > 1
                    then do
                      $info "There are several good moves ({}), re-think deeper up to {}" (nGoodMoves, maxDepth)
                      iterativeDepthDriver maxDepth Nothing nextInput'
                    else return output

    doDepthIteration :: DepthIterationInput
            -> Checkers (DepthIterationOutput, Maybe DepthIterationInput)
    doDepthIteration input@(DepthIterationInput {..}) = do
      let depth = abDepth diiParams
      if length diiMoves <= 1 -- Just one move possible
        then do
          $info "There is only one move possible; just do it." ()
          return ([MoveAndScore move score0 False | move <- diiMoves], Nothing)

        else do
          let var = aichData handle
          $info "Selecting a move. Side = {}, depth = {}, number of possible moves = {}" (show side, depth, length diiMoves)
          let opponentHasCaptures = hasCapturesOrPromotions rules (opposite side) board
          dp <- updateDepth params diiMoves opponentHasCaptures $ mkDepthParams depth diiParams
          let needDeeper = abDeeperIfBad params && isNothing diiAmbigousMovesScore && score0 `worseThan` 0
              target
                | isJust diiAmbigousMovesScore = dpTarget dp
                | otherwise = min (dpMax dp) (dpTarget dp + 1)
              dp'
                | needDeeper = dp {dpTarget = target}
                | otherwise = dp
          sortedMoves <-
              case diiSortKeys of
                Nothing -> do
                  moves' <- sortMoves eval params handle side board Nothing diiMoves
                  $info "Sort moves (init): {} => {}" (show diiMoves, show moves')
                  return moves'
                Just keys -> do
                  let moves' = map snd $ sortOn fst $ zip keys diiMoves
                  $info "Sort moves (by prev.iteration): {} => {}" (show $ zip keys diiMoves, show moves')
                  return moves'

          interval <-
              case diiAmbigousMovesScore of
                Nothing -> mkInitInterval depth (isQuiescene diiMoves)
                Just moveScore -> do
                  let d = Score 0 600
                  return (moveScore - d, moveScore + d)
          result <- widthController True True diiPrevResult sortedMoves dp' interval
          $debug "Depth iteration result: {}" (Single $ show result)
          -- In some corner cases, there might be 1 or 2 possible moves,
          -- so the timeout would allow us to calculate with very big depth;
          -- too big depth does not decide anything in such situations.
          if depth < 50
            then do
              let depthStep = abDepthStep diiParams
                  input' = deeper depth depthStep result input
              return (result, Just input')
            else return (result, Nothing)

    deeper :: Depth -> Depth -> DepthIterationOutput -> DepthIterationInput -> DepthIterationInput
    deeper depth step prevOutput input =
      let start' = fmap (+step) (abStartDepth params)
          params' = params {
                      abDepth = depth + step,
                      abDynamicDepth = abDynamicDepth params + step,
                      abStartDepth = start'
                    }
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
      let initWidth = fromIntegral (abInitWindowWidth params) :: ScoreBase
          (deltaPlus, deltaMinus)
            | depth < abDepth params =
                let d = fromIntegral $ max 1 $ abDepth params - depth
                in  (d, d)
            | not quiescene =
                let d = Score (initWidth+1) 500
                in  if maximize
                      then (d+1, d)
                      else (d, d+1)
            | isLastPiece =
                let d = Score (initWidth+1) 500
                in  if maximize
                      then (d+3, d)
                      else (d, d+3)
            | otherwise =
                let d = Score initWidth 600
                in  (d, d)
      mbPrevShift <- getLastScoreShift handle gameId
      case mbPrevShift of
        Nothing -> do
            let alpha = score0 - deltaMinus
                beta  = score0 + deltaPlus
            $debug "Score0 = {}, delta = +{}/-{} => initial interval ({}, {})" (score0, deltaPlus, deltaMinus, alpha, beta)
            return (alpha, beta)
        Just shift -> do
            let (alpha, beta)
                  | shift >= 0 = (score0 - deltaMinus, score0 + (Score shift 0) + deltaPlus)
                  | otherwise  = (score0 + (Score shift 0) - deltaMinus, score0 + deltaPlus)
            $debug "Score0 = {}, delta = +{}/-{}, shift in previous move = {} => initial interval ({}, {})"
                              (score0, deltaPlus, deltaMinus, shift, alpha, beta)
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

    nextInterval :: Bool -> (Score, Score) -> (Score, Score)
    nextInterval False (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scale` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if maximize
            then (alpha, max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), beta)
    nextInterval True (alpha, beta)
      | maximize = (alpha, win)
      | otherwise = (loose, beta)

    prevInterval :: Bool -> (Score, Score) -> (Score, Score)
    prevInterval False (alpha, beta) =
      let width = (beta - alpha)
          width' = selectScale width `scale` width
          alpha' = prevScore alpha
          beta'  = nextScore beta
      in  if minimize
            then (alpha, max beta' (beta' + width'))
            else (min alpha' (alpha' - width'), beta)
    prevInterval True (alpha, beta)
      | maximize = (loose, beta)
      | otherwise = (alpha, win)

    isAlmostLoose :: Side -> Bool
    isAlmostLoose side =
      let (men, kings) = myCounts side board
      in  (men + kings) == 1

    isLastPiece :: Bool
    isLastPiece = isAlmostLoose First || isAlmostLoose Second

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
          return [MoveAndScore move alpha False | move <- moves]
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
                        return [MoveAndScore move badScore False | move <- moves]
                      else do
                        let interval' = prevInterval (isAlmostLoose side) interval
                        $info "All moves are `too bad'; consider worse scores interval: [{} - {}]" interval'
                        widthController False True prevResult badMoves dp interval'
                  else do
                    $info "All moves are `too bad' ({}), but we have already checked worse interval; so this is the real score." (Single badScore)
                    return [MoveAndScore move badScore False | move <- moves]
              else
                case bestResults of
                  [] -> return results
                  [r] -> do
                    $info "Exactly one move ({}) is `too good'; do that move." (Single $ show $ rMove r)
                    return bestResults
                  _ ->
                    if allowNext
                      then
                        if (maximize && beta >= win) || (minimize && alpha <= loose)
                          then do
                            $info "Some moves ({} of them) are `too good'; but there is no better interval, return all of them." (Single $ length bestMoves)
                            return bestResults
                          else do
                            let interval'@(alpha',beta') = nextInterval (isAlmostLoose (opposite side)) interval
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
      let worst = if maximize then alpha else beta
      bestVar <- liftIO $ newTVarIO worst
      let inputs = [
            ScoreMoveInput {
              smiAi = ai,
              smiCache = handle,
              smiGameId = gameId,
              smiSide = side,
              smiAiSession = aiSession,
              smiIndex = index,
              smiDepth = dp,
              smiBoard = board,
              smiMove = move,
              smiAlpha = alpha,
              smiBeta = beta,
              smiBest = bestVar
            } | (move, index) <- zip moves indicies ]

          groups = [[input] | input <- inputs]
            -- | otherwise = transpose $ chunksOf nThreads inputs

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
    joinResults (Just prevResults) results = do
      let prev = M.fromList [(rMove r, r) | r <- prevResults]
      mapM (joinResult prev) results

    joinResult :: M.Map PossibleMove MoveAndScore -> Either Error MoveAndScore -> Checkers MoveAndScore
    joinResult prev (Right result)
      | rInterrupted result = do
          case M.lookup (rMove result) prev of
            Nothing -> return $ result {rScore = 0}
            Just prevR -> do
              $info "Evaluation of move {} was interrupted, use result from previous iteration: {}"
                  (show (rMove prevR), rScore prevR)
              return prevR
      | otherwise = return result
    joinResult _ (Left err) = throwError err

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
      game <- getGame gameId
      let halfMoves = gameMoveNumber game
          moveNumber = halfMoves `div` 2
          nOptions = if moveNumber <= abRandomOpeningDepth params
                       then abRandomOpeningOptions params
                       else 1
      if nOptions == 1
        then do
          let goodMoves = [rMove result | result <- pairs, rScore result == maxScore]
          return (goodMoves, maxScore)
        else do
          let srt = if maximize then sortOn (negate . rScore) else sortOn rScore
              goodMoves = map rMove $ take nOptions $ srt pairs
          return (goodMoves, maxScore)

-- | Calculate score of the board
doScore :: (GameRules rules, VectorEvaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> GameId
        -> Side
        -> AiSession
        -> DepthParams
        -> Board
        -> Score -- ^ Alpha
        -> Score -- ^ Beta
        -> Checkers ScoreOutput
doScore rules eval var params gameId side aiSession dp board alpha beta = do
    initState <- mkInitState
    out <- evalStateT (cachedScoreAB var eval params input) initState
    return out
  where
    input = ScoreInput side dp alpha beta board Nothing Nothing
    mkInitState = do
      cache <- allocateCache var eval
      return $ ScoreState rules eval cache gameId aiSession [loose] M.empty

clamp :: Ord a => a -> a -> a -> a
clamp alpha beta score
  | score < alpha = alpha
  | score > beta  = beta
  | otherwise = score

data CachedResult =
    CacheExactHit Score
  | CacheRangeHit Score Score

-- | Calculate score of the board. 
-- This uses the cache. It is called in the recursive call also.
cachedScoreAB :: forall rules eval. (GameRules rules, VectorEvaluator eval)
              => AICacheHandle rules eval
              -> eval
              -> AlphaBetaParams
              -> ScoreInput
              -> ScoreM rules eval ScoreOutput
cachedScoreAB var eval params input = do -- scoreAB var eval params input
  let depth = dpCurrent dp
      side = siSide input
      board = siBoard input
      dp = siDepth input
      alpha = siAlpha input
      beta = siBeta input
      cacheKey = mkCacheKey eval board
      cacheKey' = mkCacheKey eval (flipBoard board)
  cache <- gets ssCache
  mbItem <- lookupAiCacheS cacheKey dp cache
  mbCached <- case mbItem of
                Just item -> do
                  let cachedScore = itemScore item
                  -- it is possible that this value was put to cache with different
                  -- values of alpha/beta; but we have to maintain the property of
                  -- AB-section: alpha <= result <= beta. So here we clamp the value
                  -- that we got from cache.
                  case itemBound item of
                    Exact -> return $ CacheExactHit (clamp alpha beta cachedScore)
                    Alpha -> if cachedScore <= alpha
                               then return $ CacheExactHit alpha
                               else return $ CacheRangeHit alpha beta
                    Beta  -> if cachedScore >= beta
                               then return $ CacheExactHit beta
                               else return $ CacheRangeHit alpha beta
                Nothing -> return $ CacheRangeHit alpha beta
  case mbCached of
    CacheExactHit s -> do
        -- out <- Monitoring.timed "ai.score.board" $ scoreAB var eval params input
        -- when (soScore out /= s && s > alpha && s < beta) $
        --     $info "@{} Side {}, AB [{} - {}]: Cached value {} != actual value {}" (depth, show side, alpha, beta, s, soScore out)
        return $ ScoreOutput s False False
    CacheRangeHit alpha' beta' -> do
      out <- Monitoring.timed "ai.score.board" $ scoreAB var eval params $ input -- {siAlpha = alpha', siBeta = beta'}
      let score = soScore out
          score' = clamp alpha' beta' score
          bound
            | score <= alpha' = Alpha
            | score >= beta' = Beta
            | otherwise = Exact
          -- we can only put the result to the cache if we know
          -- that this score was not clamped by alpha or beta
          -- (so this is a real score, not alpha/beta bound)
          item = PerBoardData (dpLast dp) score' bound
          item' = PerBoardData (dpLast dp) (negate score') bound
      when (bound == Exact && soQuiescene out && not (soInterrupted out) && not (dpStaticMode dp)) $ do
          lift $ putAiCache cacheKey item var
          lift $ putAiCache cacheKey' item' var
      return $ out {soScore = score'}

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
updateDepth :: (Monad m, HasLogging m, MonadIO m) => AlphaBetaParams -> [PossibleMove] -> Bool -> DepthParams -> m DepthParams
updateDepth params moves opponentHasCaptures dp
    | nMoves == 1 = do
                  return $ dp {
                              dpCurrent = dpCurrent dp + 1,
                              dpMax = dpMax dp + 1,
                              dpForcedMode = forced || dpForcedMode dp,
                              dpStaticMode = static
                            }
    | deepen = do
                  let delta = fromIntegral nMoves - 1
                  let target = min (dpTarget dp + 1) $ max (dpTarget dp) (dpMax dp - delta)
                  let indent = replicate (fromIntegral $ 2*dpCurrent dp) ' '
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
    static = dpCurrent dp > abDynamicDepth params
    forced = any isCapture moves || any isPromotion moves || opponentHasCaptures
    deepen = if dpCurrent dp <= dpInitialTarget dp
               then nMoves <= abMovesLowBound params
               else forced

    canRazor = isQuiescene moves &&
               dpForcedMode dp &&
               not (dpReductedMode dp)

isQuiescene :: [PossibleMove] -> Bool
isQuiescene moves = not (any isCapture moves || any isPromotion moves)

getTimeout :: AlphaBetaParams -> Checkers Int
getTimeout params = do
  let localTimeout = abTimeout params
  globalTimeout <- asks (aiTimeout . gcAiConfig . csConfig)
  return $ min localTimeout globalTimeout

timeoutSignaller :: MVar () -> Int -> Checkers ()
timeoutSignaller signal timeout = liftIO $ do
    start <- getTime RealtimeCoarse
    -- TimeSpec expects nanoseconds
    let end = start + TimeSpec 0 (fromIntegral $ timeout * 1000 * 1000)
    loop end
  where
    loop end = do
      now <- getTime RealtimeCoarse
      if now >= end
        then void $ tryPutMVar signal ()
        else do
          -- threadDelay expects microseconds
          threadDelay $ 100 * 1000
          loop end

isStopSignalled :: ScoreM rules eval Bool
isStopSignalled = do
  stopSignal <- gets (aiStopSignal . ssAiSession)
  check <- liftIO $ tryReadMVar stopSignal
  case check of
    Nothing -> return False
    Just () -> return True

-- | Calculate score for the board.
-- This implements the alpha-beta section algorithm itself.
scoreAB :: forall rules eval. (GameRules rules, VectorEvaluator eval)
        => AICacheHandle rules eval
        -> eval
        -> AlphaBetaParams
        -> ScoreInput
        -> ScoreM rules eval ScoreOutput
scoreAB var eval params input
  | alpha == beta = do
      $verbose "Alpha == Beta == {}, return it" (Single $ show alpha)
      quiescene <- checkQuiescene
      return $ ScoreOutput alpha quiescene False

  | isTargetDepth dp = do
      -- target depth is achieved, calculate score of current board directly
      evaluator <- gets ssEvaluator
      let score0 = evalBoard' evaluator board
      $verbose "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, show alpha, show beta, show score0)
      quiescene <- checkQuiescene
      return $ ScoreOutput score0 quiescene False

  | otherwise = do
      evaluator <- gets ssEvaluator
      let score0 = evalBoard' evaluator board
      futilePrunned <- checkFutility
      case futilePrunned of
        Just out -> do
            $verbose "Further search is futile, return current score0 = {}" (Single $ show score0)
            return out
        Nothing -> do

          moves <- case siPossibleMoves input of
                     Nothing -> lift $ getPossibleMoves var side board
                     Just ms -> return ms
          let quiescene = isQuiescene moves
          let worst
                | maximize = alpha
                | otherwise = beta

          if null moves
            -- this actually means that corresponding side lost.
            then do
              $verbose "{}`—No moves left." (Single indent)
              return $ ScoreOutput worst True False
            else
              if dpStaticMode dp && isQuiescene moves
                -- In static mode, we are considering forced moves only.
                -- If we have reached a quiescene, then that's all.
                then do
                  $verbose "Reached quiescene in static mode; return current score0 = {}" (Single $ show score0)
                  return $ ScoreOutput score0 True False
                else do
                  -- first, let "best" be the worse possible value
                  let best
                        | dpStaticMode dp = evalBoard' evaluator board
                        | otherwise = worst

                  push best
                  $verbose "{}V Side: {}, A = {}, B = {}" (indent, show side, show alpha, show beta)
                  rules <- gets ssRules
                  let opponentHasCaptures = hasCapturesOrPromotions rules (opposite side) board
                  dp' <- updateDepth params moves opponentHasCaptures dp
                  let prevMove = siPrevMove input
                  moves' <- sortMoves eval params var side board prevMove moves
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
        then return $ Just $ ScoreOutput score0 quiescene False
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
        go [input] = cachedScoreAB var eval params input
        go (input : inputs) = do
          out <- cachedScoreAB var eval params input
          let score = soScore out
          if maximize && score >= beta || minimize && score <= alpha
            then go inputs
            else return out


    iterateMoves :: [(Int,PossibleMove, DepthParams)] -> ScoreM rules eval ScoreOutput
    iterateMoves [] = do
      best <- getBest
      $verbose "{}`—All moves considered at this level, return best = {}" (indent, show best)
      quiescene <- checkQuiescene
      -- $info "@{} All checked: {}: [{} - {}] => {}" (dpCurrent dp, show side, alpha, beta, show best)
      return $ ScoreOutput best quiescene False
    iterateMoves ((i,move, dp) : moves) = do
      stopSignal <- isStopSignalled
      if stopSignal
        then do
          best <- getBest
          quiescene <- checkQuiescene
          $info "Requested stop. Current level = {}, return best = {}" (dpCurrent dp, show best)
          return $ ScoreOutput best quiescene True
        else do
          $verbose "{}|+Check move of side {}: {}" (indent, show side, show move)
          evaluator <- gets ssEvaluator
          rules <- gets ssRules
          best <- getBest
          let board' = applyMoveActions (pmResult move) board
              side' = opposite side
              nextMoves = possibleMoves rules side' board'
              input' = input {
                          siSide = side'
                        , siAlpha = if maximize
                                     then max alpha best
                                     else alpha
                        , siBeta = if maximize
                                     then beta
                                     else min beta best
                        , siPrevMove = Just move
                        , siBoard = markAttacked nextMoves board'
                        , siPossibleMoves = Just nextMoves
                        , siDepth = dp
                      }
          out <- cachedScoreAB var eval params input'
          if soInterrupted out
            then return $ out {soScore = 0}
            else do
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
                            -- $info "@{} Section: {}: [{} - {}] => {}" (dpCurrent dp, show side, alpha, beta, show score)
                            return $ ScoreOutput score quiescene False

                       else iterateMoves moves
                else do
                     iterateMoves moves

instance (Evaluator eval, GameRules rules) => Evaluator (AlphaBeta rules eval) where
  evaluatorName (AlphaBeta _ _ eval) = evaluatorName eval
  evalBoard (AlphaBeta params rules eval) whoAsks board =
    evalBoard eval whoAsks board

