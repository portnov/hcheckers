{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Learn where

import Control.Monad
import Control.Monad.State
import qualified Control.Monad.Metrics as Metrics
import Data.Maybe
import Data.Text.Format.Heavy
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import AI.AlphaBeta
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache
import Formats.Types
import Formats.Pdn

doLearn :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules eval -> AlphaBetaParams -> GameRecord -> Checkers ()
doLearn rules eval var params gameRec = do
    let board = initBoardFromTags (SomeRules rules) (grTags gameRec)
    $info "Initial board: {}; tags: {}" (show board, show $ grTags gameRec)
    forM_ (instructionsToMoves $ grMoves gameRec) $ \moves -> do
      -- liftIO $ print moves
      go board [] moves
  where
    go _ _ [] = return ()
    go board0 predicted (moveRec : rest) = do
      (board1, predict2) <- do
        case mrFirst moveRec of
          Nothing -> return (board0, [])
          Just rec -> do
            let move1 = parseMoveRec rules First board0 rec
            if move1 `elem` predicted
              then Metrics.increment "learn.hit"
              else Metrics.increment "learn.miss"
            let (board1, _,_) = applyMove rules First move1 board0
            predict2 <- processMove rules eval var params Second move1 board1
            return (board1, predict2)
      case mrSecond moveRec of
        Nothing -> return ()
        Just rec -> do
          let move2 = parseMoveRec rules Second board1 rec
          if move2 `elem` predict2
            then Metrics.increment "learn.hit"
            else Metrics.increment "learn.miss"
          let (board2, _, _) = applyMove rules Second move2 board1
          predict1 <- processMove rules eval var params First move2 board2
          go board2 predict1 rest

processMove :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules eval -> AlphaBetaParams -> Side -> Move -> Board -> Checkers [Move]
processMove rules eval var params side move board = do
  let ai = AlphaBeta params rules eval
  (moves, score) <- runAI ai var side board
  $info "Processed: side {}, move: {}, depth: {} => score {}; we think next best moves are: {}" (show side, show move, abDepth params, show score, show moves)
  return moves

learnPdn :: (GameRules rules, Evaluator eval) => AlphaBeta rules eval -> FilePath -> Checkers ()
learnPdn ai@(AlphaBeta params rules eval) path = do
  cache <- loadAiCache scoreMove ai
  pdn <- liftIO $ parsePdnFile (Just $ SomeRules rules) path
  let n = length pdn
  forM_ (zip [1.. ] pdn) $ \(i, gameRec) -> do
    -- liftIO $ print pdn
    $info "Processing game {}/{}..." (i :: Int, n)
    doLearn rules eval cache params gameRec
    -- saveAiCache rules params cache
    return ()

