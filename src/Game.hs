{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Game where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
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

data Player =
    User String
  | forall ai. GameAi ai => AI ai

instance Show Player where
  show (User name) = name
  show (AI ai) = aiName ai

data GameStatus = New | Running
  deriving (Eq, Show, Generic)

data Game = Game {
    getGameId :: GameId
  , gState :: GameState
  , gStatus :: GameStatus
  , gRules :: SomeRules
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: [Notify]
  , gMsgbox2 :: [Notify]
  }

instance Show Game where
  show g = printf "<Game: %s, 1: %s, 2: %s>"
                  (show $ gRules g)
                  (show $ gPlayer1 g)
                  (show $ gPlayer2 g)

instance Eq Game where
  g1 == g2 = getGameId g1 == getGameId g2

data GameState = GameState {
    gsSide :: Side
  , gsCurrentBoard :: Board
  , gsHistory :: [HistoryRecord]
  }

data HistoryRecord = HistoryRecord {
    hrSide :: Side
  , hrMove :: Move
  , hrPrevBoard :: Board
  }

type GameM a = ExceptT String (State Game) a

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
  | ResultNotify {
      nDestination :: Side
    , nSource :: Side
    , nResult :: GameResult
  }
  deriving (Eq, Show, Generic)

mkGame :: GameRules rules => rules -> Int -> Maybe BoardRep -> Game
mkGame rules id mbBoardRep =
    let board = case mbBoardRep of
                  Nothing -> initBoard rules
                  Just rep -> parseBoardRep 8 rep
        st = GameState First board []
    in  Game {
          getGameId = show id,
          gState = st,
          gStatus = New,
          gRules = SomeRules rules,
          gPlayer1 = Nothing,
          gPlayer2 = Nothing,
          gMsgbox1 = [],
          gMsgbox2 = []
        }

gamePossibleMoves :: GameM [Move]
gamePossibleMoves = do
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  currentSide <- gets (gsSide . gState)
  return $ possibleMoves rules currentSide board

gameState :: GameM (Side, Board)
gameState = do
  st <- gets gState
  return (gsSide st, gsCurrentBoard st)

data GMoveRs = GMoveRs Board [Notify]

doMoveRq :: Side -> Move -> GameM GMoveRs
doMoveRq side move = do
  currentSide <- gets (gsSide . gState)
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  if side /= currentSide
    then throwError "Not your turn"
    else if move `notElem` possibleMoves rules side board
           then throwError "Not allowed move"
           else do
                let (board', _, _) = applyMove side move board
                    moveMsg = MoveNotify (opposite side) side (moveRep side move) (boardRep board')
                    result = getGameResult rules board'
                    resultMsg to = ResultNotify to side result
                    messages = if result == Ongoing
                                 then [moveMsg]
                                 else [moveMsg, resultMsg First, resultMsg Second]
                modify $ \game -> game {gState = pushMove move board' (gState game)}
                return $ GMoveRs board' messages

doMoveRepRq :: Side -> MoveRep -> GameM GMoveRs
doMoveRepRq side moveRep = do
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  case parseMoveRep rules side board moveRep of
    NoSuchMove -> throwError "No such move"
    AmbigousMove moves -> throwError $ "Move specification is ambigous. Possible moves: " ++ show moves
    Parsed move -> doMoveRq side move

data GUndoRs = GUndoRs Board [Notify]

doUndoRq :: Side -> GameM GUndoRs
doUndoRq side = do
  currentSide <- gets (gsSide . gState)
  st <- gets gState
  if side /= currentSide
    then throwError "Not your turn"
    else case popMove st of
           Nothing -> throwError "Nothing to undo"
           Just (prevBoard, prevSt) -> do
             let push = UndoNotify (opposite side) side (boardRep prevBoard)
             modify $ \game -> game {gState = prevSt}
             return $ GUndoRs prevBoard [push]

pushMove :: Move -> Board -> GameState -> GameState
pushMove move board st =
  st {
    gsSide = opposite (gsSide st),
    gsCurrentBoard = board,
    gsHistory = HistoryRecord (gsSide st) move (gsCurrentBoard st) : gsHistory st
  }

popMove :: GameState -> Maybe (Board, GameState)
popMove st =
  case gsHistory st of
    (_ : prevRecord : prevHistory) ->
      let board = hrPrevBoard prevRecord
          st' = st {gsCurrentBoard = board, gsHistory = prevHistory}
      in  Just (board, st')
    _ -> Nothing

