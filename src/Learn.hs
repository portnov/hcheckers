{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Learn where

import Control.Monad
import Control.Monad.State
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import AI.AlphaBeta
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache
import Formats.Types
import Formats.Pdn

doLearn :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules -> AlphaBetaParams -> GameRecord -> Int -> Checkers ()
doLearn rules eval var params gameRec depth = do
    let board = initBoard rules
    go board $ grMoves gameRec
  where
    go _ [] = return ()
    go board0 (moveRec : rest) = do
      let move1 = parseMoveRec rules First board0 (mrFirst moveRec)
      let (board1, _,_) = applyMove rules First move1 board0
      processMove rules eval var params Second depth move1 board1
      case mrSecond moveRec of
        Nothing -> return ()
        Just rec -> do
          let move2 = parseMoveRec rules Second board1 rec
          let (board2, _, _) = applyMove rules Second move2 board1
          processMove rules eval var params First depth move2 board2
          go board2 rest

parseMoveRec :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Move
parseMoveRec rules side board rec =
  let moves = possibleMoves rules side board
      suits m = aLabel (pmBegin m) == smrFrom rec &&
                aLabel (pmEnd m) == smrTo rec &&
                (not $ null $ pmVictims m) == smrCapture rec
  in case filter suits moves of
    [m] -> pmMove m
    [] -> error $ "no such move: " ++ show rec
    ms -> error $ "ambigous move: " ++ show ms

processMove :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules -> AlphaBetaParams -> Side -> Int -> Move -> Board -> Checkers ()
processMove rules eval var params side depth move board = do
  let ai = AlphaBeta params rules
  (moves, score) <- runAI ai var side board
  $info "Processed: side {}, move: {}, depth: {} => score {}; we think next best moves are: {}" (show side, show move, depth, score, show moves)
  return ()

learnPdn :: (GameRules rules) => AlphaBeta rules -> FilePath -> Int -> Checkers ()
learnPdn ai@(AlphaBeta params rules) path depth = do
  cache <- loadAiCache scoreMove ai
  pdn <- liftIO $ parsePdn path
  forM_ pdn $ \gameRec -> do
    doLearn rules ai cache params gameRec depth
    -- saveAiCache rules params cache
    return ()

