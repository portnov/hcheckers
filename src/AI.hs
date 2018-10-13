{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module AI where

import Control.Monad
import Control.Monad.State
import Control.Exception (evaluate)
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Data.Aeson
import Text.Printf
import System.Clock

import Types
import Board
import BoardMap
-- import Russian

-- import Debug.Trace

trace :: String -> x -> x
trace _ x = x

data AlphaBetaParams = AlphaBetaParams Int
  deriving (Show)

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"

data AlphaBeta rules eval = AlphaBeta Int rules eval
  deriving (Show)

instance (GameRules rules, Evaluator eval) => GameAi (AlphaBeta rules eval) where
  chooseMove (AlphaBeta depth rules eval) side board = do
    let moves = possibleMoves rules side board
    if null moves
      then return Nothing
      else do
          scores <- forM moves $ \move -> do
                       printf "Check: %s\n" (show move)
                       time1 <- getTime Realtime
                       let (board', _, _) = applyMove side move board
                       score <- evaluate $ doScore rules eval (opposite side) depth board'
                       time2 <- getTime Realtime
                       let delta = time2-time1
                       printf " => %d (in %ds + %dns)\n" score (sec delta) (nsec delta)
                       return (move, score)
          let select = if side == First then maximum else minimum
              maxScore = select $ map snd scores
              goodMoves = [move | (move, score) <- scores, score == maxScore]
          let move = head goodMoves
          printf "AI move for side %s: %s, score is %d\n" (show side) (show move) maxScore
          return $ Just move

  updateAi ai@(AlphaBeta depth rules eval) json =
    case fromJSON json of
      Error _ -> ai
      Success (AlphaBetaParams depth') -> AlphaBeta depth' rules eval

max_value :: Score
max_value = 1000000

-- type ScoreMemo = M.Map Side (M.Map Int (M.Map Score (M.Map Score Score)))

type MovesMemo = M.Map Side (BoardMap [Move])

lookupMoves :: Side -> Board -> MovesMemo -> Maybe [Move]
lookupMoves side board memo =
  lookupBoardMap board =<< M.lookup side memo

putMoves :: Side -> Board -> [Move] -> MovesMemo -> MovesMemo
putMoves side board moves memo =
    M.unionWith M.union init memo
  where
    init = M.singleton side $ singleBoardMap board moves

-- type MovesMemo = M.Map Side (M.Map BoardRep [Move])
-- 
-- lookupMoves :: Side -> Board -> MovesMemo -> Maybe [Move]
-- lookupMoves side board memo =
--   M.lookup (boardRep board) =<< M.lookup side memo
-- 
-- putMoves :: Side -> Board -> [Move] -> MovesMemo -> MovesMemo
-- putMoves side board moves memo =
--     M.unionWith M.union init memo
--   where
--     init = M.singleton side $ M.singleton (boardRep board) moves

-- lookupMemo :: Side -> Int -> Score -> Score -> ScoreMemo -> Maybe Score
-- lookupMemo side depth alpha beta memo =
--   M.lookup beta =<< M.lookup alpha =<< M.lookup depth =<< M.lookup side memo
-- 
-- putMemo :: Side -> Int -> Score -> Score -> Score -> ScoreMemo -> ScoreMemo
-- putMemo side depth alpha beta score memo =
--     M.unionWith (M.unionWith (M.unionWith (M.union))) init memo
--   where
--     init = M.singleton side $ M.singleton depth $ M.singleton alpha $ M.singleton beta score

doScore :: (GameRules rules, Evaluator eval) => rules -> eval -> Side -> Int -> Board -> Score
doScore rules eval side depth board = fixSign $ evalState (scoreAB side depth (-max_value) max_value) initState
  where
    initState = ScoreState rules eval [StackItem Nothing board score0 (-max_value)] M.empty
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

type ScoreM rules eval a = State (ScoreState rules eval) a

scoreAB :: forall rules eval. (GameRules rules, Evaluator eval) => Side -> Int -> Score -> Score -> ScoreM rules eval Score
scoreAB side 0 alpha beta = do
    score0 <- gets (siScore0 . head . ssStack)
    -- let score0' = if side == First then score0 else -score0
    trace (printf "    X Side: %s, A = %d, B = %d, score0 = %d" (show side) alpha beta score0) $ return ()
    return score0
scoreAB side depth alpha beta = do
    rules <- gets ssRules
    setBest $ if maximize then alpha else beta -- we assume alpha <= beta
    trace (printf "%sV Side: %s, A = %d, B = %d" indent (show side) alpha beta) $ return ()
    board <- getBoard
    moves <- possibleMoves' board

    when (null moves) $
      trace (printf "%s| No moves left." indent) $ return ()
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
      trace (printf "%s| side %s: %s" indent (show side) (showStack stack)) $ return ()

    indent = replicate (2*(4-depth)) ' '

    getBest =
      gets (siScoreBest . head . ssStack)

    setBest :: Score -> ScoreM rules eval ()
    setBest best = do
      oldBest <- getBest
      trace (printf "%s| %s for depth %d : %d => %d" indent bestStr depth oldBest best) $ return ()
      modify $ \st -> st {ssStack = update (head $ ssStack st) : tail (ssStack st)}
        where
          update item = item {siScoreBest = best}

    iterateMoves [] = do
      best <- getBest
      trace (printf "%s|—All moves considered at this level, return best = %d" indent best) $ return ()
      return best
    iterateMoves (move : moves) = do
      trace (printf "%s|+Check move of side %s: %s" indent (show side) (show move)) $ return ()
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
      score <- scoreAB (opposite side) (depth - 1) alpha' beta'
      trace (printf "%s| score for side %s: %d" indent (show side) score) $ return ()
      pop
      best <- getBest

      -- trace (printf "%s| Score for depth %d = %d, prev.best = %d" indent depth score best) $ return ()
      if (maximize && score > best) || (minimize && score < best)
        then do
             setBest score
             if score >= beta
               then do
                    best <- getBest
                    trace (printf "%s| Return %s for depth %d = %d" indent bestStr depth best) $ return ()
                    return best
                    
               else iterateMoves moves
        else do
             -- trace (printf "%s| Score for side %s = %d, go to next move." indent (show side) score) $ return ()
             iterateMoves moves
        
