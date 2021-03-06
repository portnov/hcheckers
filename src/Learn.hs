{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Learn where

import Control.Monad
import Control.Monad.State
import Control.Concurrent.STM
import qualified Control.Monad.Metrics as Metrics
import Data.Text.Format.Heavy
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.Supervisor (newAiSession)
import AI.AlphaBeta
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache
-- import AI.AlphaBeta.Persistent
import Formats.Types
import Formats.Pdn

doLearn' :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules eval -> AlphaBetaParams -> GameRecord -> Checkers ()
doLearn' rules eval var params gameRec = do
    sup <- askSupervisor
    supervisor <- liftIO $ atomically $ readTVar sup
    let startBoard = initBoardFromTags supervisor (SomeRules rules) (grTags gameRec)
    let result = resultFromTags $ grTags gameRec
    $info "Initial board: {}; result: {}" (show startBoard, show result)
    forM_ (instructionsToMoves $ grMoves gameRec) $ \moves -> do
        let (endScore, allBoards) = go [] startBoard result moves
        $info "End score: {}" (Single endScore)
  where
    go boards lastBoard (Just result) [] = (resultToScore result, lastBoard : boards)
    go boards lastBoard Nothing [] =
      let score = evalBoard eval First lastBoard
      in  (score, lastBoard : boards)
    go boards board0 mbResult (moveRec : rest) =
      let board1 = case mrFirst moveRec of
                     Nothing -> board0
                     Just rec ->
                      let Right move1 = parseMoveRec rules First board0 rec
                          (board1, _, _) = applyMove rules First move1 board0
                      in board1
          board2 = case mrSecond moveRec of
                     Nothing -> board1
                     Just rec ->
                      let Right move2 = parseMoveRec rules Second board1 rec
                          (board2, _, _) = applyMove rules Second move2 board1
                      in board2
      in  go (board1 : boards) board2 mbResult rest

    resultToScore FirstWin = win
    resultToScore SecondWin = loose
    resultToScore Draw = 0

doLearn :: (GameRules rules, Evaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> GameId
        -> GameRecord
        -> Checkers ()
doLearn rules eval var params gameId gameRec = do
    sup <- askSupervisor
    supervisor <- liftIO $ atomically $ readTVar sup
    let startBoard = initBoardFromTags supervisor (SomeRules rules) (grTags gameRec)
    $info "Initial board: {}; tags: {}" (show startBoard, show $ grTags gameRec)
    forM_ (instructionsToMoves $ grMoves gameRec) $ \moves -> do
      (endScore, allBoards) <- go (0, []) startBoard [] moves
      $info "End score: {}" (Single endScore)

  where
    go (score, boards) lastBoard _ [] = return (score, lastBoard : boards)
    go (score0, boards) board0 predicted (moveRec : rest) = do
      (board1, predict2, score2) <- do
        case mrFirst moveRec of
          Nothing -> return (board0, [], score0)
          Just rec -> do
            let Right move1 = parseMoveRec rules First board0 rec
            if move1 `elem` map pmMove predicted
              then Metrics.increment "learn.hit"
              else Metrics.increment "learn.miss"
            let (board1, _,_) = applyMove rules First move1 board0
            (predict2, score2) <- processMove rules eval var params gameId Second move1 board1
            return (board1, predict2, score2)
      case mrSecond moveRec of
        Nothing -> return (score2, board0 : board1 : boards)
        Just rec -> do
          let Right move2 = parseMoveRec rules Second board1 rec
          if move2 `elem` map pmMove predict2
            then Metrics.increment "learn.hit"
            else Metrics.increment "learn.miss"
          let (board2, _, _) = applyMove rules Second move2 board1
          (predict1, score1) <- processMove rules eval var params gameId First move2 board2
          go (score1, board0 : board1 : boards) board2 predict1 rest

processMove :: (GameRules rules, Evaluator eval)
            => rules
            -> eval
            -> AICacheHandle rules eval
            -> AlphaBetaParams
            -> GameId
            -> Side
            -> Move
            -> Board
            -> Checkers ([PossibleMove], Score)
processMove rules eval var params gameId side move board = do
  let ai = AlphaBeta params rules eval
  (sessionId, newSession) <- newAiSession
  (moves, score) <- runAI ai var gameId side newSession board
  $info "Processed: side {}, move: {}, depth: {} => score {}; we think next best moves are: {}" (show side, show move, abDepth params, show score, show moves)
  return (moves, score)

learnPdn :: (GameRules rules, VectorEvaluator eval) => AlphaBeta rules eval -> FilePath -> Checkers ()
learnPdn ai@(AlphaBeta params rules eval) path = do
  cache <- loadAiCache scoreMoveGroup ai
  pdn <- liftIO $ parsePdnFile (Just $ SomeRules rules) path
  let n = length pdn
  forM_ (zip [1.. ] pdn) $ \(i, gameRec) -> do
    -- liftIO $ print pdn
    $info "Processing game {}/{}..." (i :: Int, n)
    doLearn rules eval cache params (show i) gameRec
    -- saveAiCache rules params cache
    return ()

