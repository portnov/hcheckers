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

getGameId :: GameHandle -> GameId
getGameId h = show (gThread h)

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
  | GUndoRq Side
  | GStateRq

data GameRs =
    Ok
  | DoMoveRs Board [Notify]
  | GPossibleMovesRs [Move]
  | GStateRs Side Board
  | GUndoRs Board [Notify]
  | Error String
  deriving (Show)

data Notify =
    MoveNotify {
      nDestination :: Side
    , nSource :: Side
    , nMove :: MoveRep
    , nBoard :: BoardRep
    }
  | UndoNotify {
      nDestination :: Side
    , nSource :: Side
    , nBoard :: BoardRep
    }
  deriving (Eq, Show, Generic)

spawnGame :: GameRules rules => rules -> Maybe BoardRep -> IO GameHandle
spawnGame rules mbBoardRep = do
    input <- newChan
    output <- newChan
    thread <- forkIO (setup input output)
    return $ GameHandle thread input output

  where
    setup input output = do
      let board = case mbBoardRep of
                    Nothing -> initBoard rules
                    Just rep -> parseBoardRep 8 rep
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
          writeChan (gsOutput st) $ GStateRs (gsSide st) $ gsCurrentBoard st
          loop st

        DoMoveRq s move -> processMove s move st

        DoMoveRepRq s moveRep ->
          case parseMoveRep rules s (gsCurrentBoard st) moveRep of
            NoSuchMove -> do
                       writeChan (gsOutput st) (Error "No such move")
                       loop st
            AmbigousMove moves -> do
                       writeChan (gsOutput st) (Error $ "Move specification is ambigous. Possible moves: " ++ show moves)
                       loop st
            Parsed move -> processMove s move st

        GUndoRq side -> do
          if side /= gsSide st
            then do
                 writeChan (gsOutput st) (Error "Not your turn")
                 loop st
            else do
                 case popMove st of
                   Nothing -> do
                     writeChan (gsOutput st) (Error "Nothing to undo")
                     loop st
                   Just (prevBoard, prevSt) -> do
                     let push = UndoNotify (opposite side) side (boardRep prevBoard)
                     writeChan (gsOutput st) $ GUndoRs prevBoard [push]
                     loop prevSt

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
                        let side = gsSide st
                            (board', _, _) = applyMove side move (gsCurrentBoard st)
                            push = MoveNotify (opposite side) side (moveRep side move) (boardRep board')
                        writeChan (gsOutput st) $ DoMoveRs board' [push]
                        loop $ pushMove move board' st
    
    popMove st =
      case gsHistory st of
        (_ : prevRecord : prevHistory) ->
          let board = hrPrevBoard prevRecord
              st' = st {gsCurrentBoard = board, gsHistory = prevHistory}
          in  Just (board, st')
        _ -> Nothing

      

