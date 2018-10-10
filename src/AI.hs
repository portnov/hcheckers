{-# LANGUAGE ScopedTypeVariables #-}
module AI where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf

import Types
import Board
-- import Russian

import Debug.Trace

data AlphaBeta rules eval = AlphaBeta Int rules eval
  deriving (Show)

instance (GameRules rules, Evaluator eval) => GameAi (AlphaBeta rules eval) where
  chooseMove (AlphaBeta depth rules eval) side board = do
    let moves = possibleMoves rules side board
    scores <- forM moves $ \move -> do
                 printf "Check: %s\n" (show move)
                 let (board', _, _) = applyMove side move board
                     score = doScore rules eval (opposite side) depth board'
                 printf " => %d\n" score
                 return (move, score)
    let select = if side == First then maximum else minimum
        maxScore = select $ map snd scores
        goodMoves = [move | (move, score) <- scores, score == maxScore]
    return $ head goodMoves

max_value :: Integer
max_value = 1000000

doScore :: (GameRules rules, Evaluator eval) => rules -> eval -> Side -> Int -> Board -> Integer
doScore rules eval side depth board = fixSign $ evalState (scoreAB side side depth (-max_value) max_value) initState
  where
    initState = ScoreState rules eval [StackItem Nothing board score0 (-max_value)]
    score0 = evalBoard eval First board

    fixSign s
      | side == First = s
      | otherwise = negate s

data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssStack :: [StackItem]
  }

data StackItem = StackItem {
    siMove :: Maybe Move
  , siBoard :: Board
  , siScore0 :: Integer
  , siScoreBest :: Integer
  }

instance Show StackItem where
  show si = printf "(%s : %d)" (showM (siMove si)) (siScore0 si)
    where
      showM Nothing = "Start"
      showM (Just move) = show move

type Score rules eval a = State (ScoreState rules eval) a

scoreAB :: forall rules eval. (GameRules rules, Evaluator eval) => Side -> Side -> Int -> Integer -> Integer -> Score rules eval Integer
scoreAB _ side 0 alpha beta = do
    score0 <- gets (siScore0 . head . ssStack)
    let score0' = if side == First then score0 else -score0
    trace (printf "    X Side: %s, A = %d, B = %d, score0 = %d" (show side) alpha beta score0') $ return ()
    return score0'
scoreAB initSide side depth alpha beta = do
    rules <- gets ssRules
    setBest alpha
    trace (printf "%sV Side: %s, A = %d, B = %d" indent (show side) alpha beta) $ return ()
    board <- getBoard
    let moves = possibleMoves rules side board

    when (null moves) $
      trace (printf "%s| No moves left." indent) $ return ()
    iterateMoves moves

  where
    
    push :: Move -> Board -> Integer -> Score rules eval ()
    push move board score =
      modify $ \st -> st {ssStack = (StackItem (Just move) board score (-max_value)) : (ssStack st)}

    pop = 
      modify $ \st -> st {ssStack = tail (ssStack st)}

    getBoard =
      gets (siBoard . head . ssStack)

    getScore0 :: Score rules eval Integer
    getScore0 =
      gets (siScore0 . head . ssStack)

    showStack s = unwords $ map show s

    printStack = do
      stack <- gets (reverse . ssStack)
      trace (printf "%s| side %s: %s" indent (show side) (showStack stack)) $ return ()

    indent = replicate (2*(2-depth)) ' '

    getBest =
      gets (siScoreBest . head . ssStack)

    setBest :: Integer -> Score rules eval ()
    setBest best = do
      trace (printf "%s| Best for depth %d := %d" indent depth best) $ return ()
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
      let score0 = evalBoard evaluator First board'
      best <- getBest
      push move board' score0
      printStack
      score <- negate <$> scoreAB initSide (opposite side) (depth - 1) (negate beta) (negate best)
      trace (printf "%s| score for side %s: %d" indent (show side) score) $ return ()
      pop
      best <- getBest

      trace (printf "%s| Score for depth %d = %d, prev.best = %d" indent depth score best) $ return ()
      if score > best
        then do
             setBest score
             if score >= beta
               then do
                    best <- getBest
                    trace (printf "%s| Return best for depth %d = %d" indent depth best) $ return ()
                    return best
                    
               else iterateMoves moves
        else do
             -- trace (printf "%s| Score for side %s = %d, go to next move." indent (show side) score) $ return ()
             iterateMoves moves
        
