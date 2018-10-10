{-# LANGUAGE DeriveGeneric #-}

module Game where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf
import Data.IORef
import GHC.Generics

import Types
import Board
-- import Russian

import Debug.Trace

type GameId = String

data GameHandle = GameHandle {
    gThread :: ThreadId
  , gInput :: Chan GameRq
  , gOutput :: Chan GameRs
  }

data GameState = GameState {
    gsInput :: Chan GameRq
  , gsOutput :: Chan GameRs
  , gsSide :: Side
  , gsCurrentBoard :: Board
  , gsHistory :: [HistoryRecord]
  }

data HistoryRecord = HistoryRecord {
    hrSide :: Side
  , hrMove :: Move
  , hrPrevBoard :: Board
  }

data GameRq =
    Exit
  | DoMoveRq Side Move
  | DoMoveRepRq Side MoveRep
  | GPossibleMovesRq Side
  | GStateRq

data GameRs =
    Ok
  | DoMoveRs Board [Notify]
  | GPossibleMovesRs [Move]
  | GStateRs Side BoardRep
  | Error String

data Notify = Notify Side MoveRep BoardRep
  deriving (Eq, Show, Generic)

spawnGame :: GameRules rules => rules -> IO GameHandle
spawnGame rules = do
    input <- newChan
    output <- newChan
    thread <- forkIO (setup input output)
    return $ GameHandle thread input output

  where
    setup input output = do
      let board = initBoard rules
      let side = First
      loop $ GameState input output side board []

    loop st = do
      rq <- readChan (gsInput st)
      case rq of
        Exit -> writeChan (gsOutput st) Ok

        GPossibleMovesRq s -> do
          let moves = possibleMoves rules s (gsCurrentBoard st)
          writeChan (gsOutput st) (GPossibleMovesRs moves)
          loop st

        GStateRq -> do
          writeChan (gsOutput st) $ GStateRs (gsSide st) $ boardRep $ gsCurrentBoard st
          loop st

        DoMoveRq s move -> processMove s move st

        DoMoveRepRq s moveRep ->
          case parseMoveRep rules s (gsCurrentBoard st) moveRep of
            Nothing -> do
                       writeChan (gsOutput st) (Error "Cannot parse move request")
                       loop st
            Just move -> processMove s move st

    pushMove move board st =
      st {
        gsSide = opposite (gsSide st),
        gsCurrentBoard = board,
        gsHistory = HistoryRecord (gsSide st) move (gsCurrentBoard st) : gsHistory st
      }

    processMove s move st =
          if s /= gsSide st
            then do
                 writeChan (gsOutput st) (Error "Not your turn")
                 loop st
            else do
                 if move `notElem` possibleMoves rules (gsSide st) (gsCurrentBoard st)
                   then do
                        writeChan (gsOutput st) (Error "Not allowed move")
                        loop st
                   else do
                        let (board', _, _) = applyMove (gsSide st) move (gsCurrentBoard st)
                            push = Notify (opposite $ gsSide st) (moveRep move) (boardRep board')
                        writeChan (gsOutput st) $ DoMoveRs board' [push]
                        loop $ pushMove move board' st

      

