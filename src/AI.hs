{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module AI where

import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import Data.Maybe
import Data.Text.Format.Heavy
import Data.Aeson
import Text.Printf
import System.Log.Heavy
import System.Log.Heavy.TH

import Types
import Board
import BoardMap
import AICache
import Parallel

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"
      <*> v .:? "start_depth"
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
      saveAiCache rules params cache
      return ()

  chooseMove ai storage side board = do
    (moves, _) <- runAI ai storage side board
    return moves

  updateAi ai@(AlphaBeta _ rules) json =
    case fromJSON json of
      Error _ -> ai
      Success params -> AlphaBeta params rules

  aiName _ = "default"

scoreMove :: (GameRules rules) => ScoreMoveInput rules -> Checkers (Move, Score)
scoreMove (ai@(AlphaBeta params rules), var, side, depth, board, move) = do
     score <- timed "Checking a move" $ do
                let (board', _, _) = applyMove side move board
                score <- doScore rules ai var params (opposite side) depth board'
                $info "Check: {} (depth {}) => {}" (show move, depth, score)
                return score
     
     return (move, score)


runAI :: GameRules rules => AlphaBeta rules -> AICacheHandle rules -> Side -> Board -> Checkers ([Move], Score)
runAI ai@(AlphaBeta params rules) var side board = do
    let depth = abDepth params
        startDepth = fromMaybe depth (abStartDepth params)
    let moves = possibleMoves rules side board
    if null moves
      then return ([], -max_value)
      else go startDepth depth moves
  where
    go depth maxDepth moves = do
      (goodMoves, maxScore) <- runAI' depth moves
      if length goodMoves < 3
        then return (goodMoves, maxScore)
        else let depth' = depth + 2
             in  if depth' <= maxDepth
                   then go depth' maxDepth goodMoves
                   else return (goodMoves, maxScore)

    runAI' depth moves = do
          AICache _ processor _ <- liftIO $ atomically $ readTVar var
          let inputs = [(ai, var, side, depth, board, move) | move <- moves]
          scores <- process processor inputs
          let select = if side == First then maximum else minimum
              maxScore = select $ map snd scores
              goodMoves = [move | (move, score) <- scores, score == maxScore]
          return (goodMoves, maxScore)

max_value :: Score
max_value = 1000000

-- type ScoreMemo = M.Map Side (M.Map Int (M.Map Score (M.Map Score Score)))

data MovesMemo = MovesMemo {
    mmFirst :: BoardMap [Move],
    mmSecond :: BoardMap [Move]
  }

lookupMoves :: Side -> Board -> MovesMemo -> Maybe [Move]
lookupMoves First board memo = lookupBoardMap board (mmFirst memo)
lookupMoves Second board memo = lookupBoardMap board (mmSecond memo)

putMoves :: Side -> Board -> [Move] -> MovesMemo -> MovesMemo
putMoves First board moves memo =
  memo {mmFirst = putBoardMap board moves (mmFirst memo)}
putMoves Second board moves memo =
  memo {mmSecond = putBoardMap board moves (mmSecond memo)}

doScore :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules -> AlphaBetaParams -> Side -> Int -> Board -> Checkers Score
doScore rules eval var params side depth board =
    fixSign <$> evalStateT (cachedScoreAB var params side depth (-max_value) max_value) initState
  where
    initState = ScoreState rules eval [StackItem Nothing board score0 (-max_value)] emptyMemo
    emptyMemo = MovesMemo emptyBoardMap emptyBoardMap
    score0 = evalBoard eval First (opposite side) board

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
    siMove :: Maybe Move
  , siBoard :: ! Board
  , siScore0 :: ! Score
  , siScoreBest :: ! Score
  }

instance Show StackItem where
  show si = printf "(%s : %d)" (showM (siMove si)) (siScore0 si)
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
              -> Int
              -> Score
              -> Score
              -> ScoreM rules eval Score
cachedScoreAB var params side depth alpha beta = do
  board <- gets (siBoard . head . ssStack)
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
      (score, moves) <- scoreAB var params side depth alpha beta 
      lift $ putAiCache params board depth side score moves var
      return score

scoreAB :: forall rules eval. (GameRules rules, Evaluator eval)
        => AICacheHandle rules
        -> AlphaBetaParams
        -> Side
        -> Int
        -> Score
        -> Score
        -> ScoreM rules eval (Score, [Move])
scoreAB _ _ side 0 alpha beta = do
    score0 <- gets (siScore0 . head . ssStack)
    -- let score0' = if side == First then score0 else -score0
    $trace "    X Side: {}, A = {}, B = {}, score0 = {}" (show side, alpha, beta, score0)
    return (score0, [])
scoreAB var params side depth alpha beta = do
    rules <- gets ssRules
    setBest $ if maximize then alpha else beta -- we assume alpha <= beta
    $trace "{}V Side: {}, A = {}, B = {}" (indent, show side, alpha, beta)
    board <- getBoard
    moves <- possibleMoves' board

    when (null moves) $
      $trace "{}| No moves left." (Single indent)
    iterateMoves moves

  where

    possibleMoves' :: Board -> ScoreM rules eval [Move]
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
    
    push :: Move -> Board -> Score -> ScoreM rules eval ()
    push move board score =
      modify $ \st -> st {ssStack = (StackItem (Just move) board score (-max_value)) : (ssStack st)}

    pop = 
      modify $ \st -> st {ssStack = tail (ssStack st)}

    getBoard =
      gets (siBoard . head . ssStack)

    getScore0 :: ScoreM rules eval Score
    getScore0 =
      gets (siScore0 . head . ssStack)

    showStack s = unwords $ map show s

    printStack = do
      stack <- gets (reverse . ssStack)
      $trace "{}| side {}: {}" (indent, show side, showStack stack)

    indent = replicate (2*(4-depth)) ' '

    getBest =
      gets (siScoreBest . head . ssStack)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      $trace "{}| {} for depth {} : {} => {}" (indent, bestStr, depth, oldBest, best)
      modify $ \st -> st {ssStack = update (head $ ssStack st) : tail (ssStack st)}
        where
          update item = item {siScoreBest = best}

    iterateMoves [] = do
      best <- getBest
      $trace "{}|—All moves considered at this level, return best = {}" (indent, best)
      return (best, [])
    iterateMoves (move : moves) = do
      $trace "{}|+Check move of side {}: {}" (indent, show side, show move)
      board <- getBoard
      evaluator <- gets ssEvaluator
      let (board', _, _) = applyMove side move board
      let score0 = evalBoard evaluator First (opposite side) board' -- next move will be done by another side
      best <- getBest
      push move board' score0
      printStack
      let alpha' = if maximize
                     then max alpha best
                     else alpha
          beta'  = if maximize
                     then beta
                     else min beta best
      score <- cachedScoreAB var params (opposite side) (depth - 1) alpha' beta'
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
                    $trace "{}| Return {} for depth {} = {}" (indent, bestStr, depth, best)
                    return (best, [])
                    
               else iterateMoves moves
        else do
             -- trace (printf "%s| Score for side %s = %d, go to next move." indent (show side) score) $ return ()
             iterateMoves moves
        
win :: Score
win = max_value

captureManCoef :: Int
captureManCoef = 10

captureKingCoef :: Int
captureKingCoef = 50

kingCoef :: Int
kingCoef = 5

instance GameRules rules => Evaluator (AlphaBeta rules) where
  evaluatorName _ = "russian"

  evalBoard ai@(AlphaBeta _ rules) whoAsks whoMovesNext board =
    let (myMen, myKings) = myCounts whoAsks board
        (opponentMen, opponentKings) = myCounts (opposite whoAsks) board
        myScore = kingCoef * myKings + myMen
        opponentScore = kingCoef * opponentKings + opponentMen
    in  if myMen == 0 && myKings == 0
          then -win
          else if opponentMen == 0 && opponentKings == 0
                 then win
                 else fromIntegral $ myScore - opponentScore

--     let allMyMoves = possibleMoves rules whoAsks board
--         allOpponentMoves = possibleMoves rules (opposite whoAsks) board
-- 
--         myMoves = if whoAsks == whoMovesNext
--                     then allMyMoves
--                     else filter (not . isCapture) allMyMoves
--         opponentMoves = if whoAsks == whoMovesNext
--                           then filter (not . isCapture) allOpponentMoves
--                           else allOpponentMoves
-- 
--     in  if null allMyMoves
--           then {- trace (printf "Side %s loses" (show whoAsks)) -} (-win)
--           else if null allOpponentMoves
--                  then {-  trace (printf "Side %s wins" (show whoAsks)) -} win
--                  else let movesScore s ms = if all isCapture ms
--                                                then let (men, kings) = unzip [capturesCounts move board | move <- ms]
--                                                         maxMen = if null men then 0 else maximum men
--                                                         maxKings = if null kings then 0 else maximum kings
--                                                     in  fromIntegral $
-- --                                                         trace (printf "Side %s possible captures: %s men, %s kings" (show s) (show men) (show kings)) $
--                                                         captureManCoef * maxMen + captureKingCoef * maxKings
--                                                else fromIntegral $ length ms
--                           myMovesScore = movesScore whoAsks myMoves
--                           opponentMovesScore = movesScore (opposite whoAsks) opponentMoves
--                       in --  trace (printf "Side %s moves score %d, opponent moves score %d, total score = %d" (show whoAsks) myMovesScore opponentMovesScore (myMovesScore - opponentMovesScore)) $
--                           myMovesScore - opponentMovesScore
-- 
