
module Learn where

import Control.Monad
import Control.Monad.State
import Control.Exception (evaluate)
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Typeable
import Data.Ord
import Data.List
import Data.Aeson
import Text.Printf
import System.Clock

import Types
import Board
import BoardMap
import AI
import AICache
import Pdn

doLearn :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle -> AlphaBetaParams -> GameRecord -> Int -> IO ()
doLearn rules eval var params gameRec depth = do
    let board = initBoard rules
    go board $ grMoves gameRec
  where
    go _ [] = return ()
    go board0 (moveRec : rest) = do
      let move1 = parseMoveRec rules First board0 (mrFirst moveRec)
      let (board1, _,_) = applyMove First move1 board0
      processMove rules eval var params Second depth move1 board1
      case mrSecond moveRec of
        Nothing -> return ()
        Just rec -> do
          let move2 = parseMoveRec rules Second board1 rec
          let (board2, _, _) = applyMove Second move2 board1
          processMove rules eval var params First depth move2 board2
          go board2 rest

parseMoveRec :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Move
parseMoveRec rules side board rec =
  let moves = possibleMoves rules side board
      suits m = aLabel (moveBegin m) == smrFrom rec &&
                aLabel (moveEnd side board m) == smrTo rec &&
                isCapture m == smrCapture rec
  in case filter suits moves of
    [m] -> m
    [] -> error $ "no such move: " ++ show rec
    ms -> error $ "ambigous move: " ++ show ms

processMove :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle -> AlphaBetaParams -> Side -> Int -> Move -> Board -> IO ()
processMove rules eval var params side depth move board = do
  let ai = AlphaBeta params rules
  (moves, score) <- runAI ai var side board
  printf "Processed: side %s, move: %s, depth: %d => score %d; we think next best moves are: %s\n" (show side) (show move) depth score (show moves)
  return ()

learnPdn :: (GameRules rules, Evaluator eval) => rules -> eval -> AlphaBetaParams -> FilePath -> Int -> IO ()
learnPdn rules eval params path depth = do
  cache <- loadAiCache rules params
  pdn <- parsePdn path
  forM_ pdn $ \gameRec -> do
    doLearn rules eval cache params gameRec depth
    saveAiCache rules params cache
    return ()

