{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Core.Game where

import Control.Monad.State
import Control.Monad.Except

import Core.Types
import Core.Board

-- import Debug.Trace

type GameM a = ExceptT Error (State Game) a

mkGame :: GameRules rules => rules -> Int -> Maybe BoardRep -> Game
mkGame rules id mbBoardRep =
    let board = case mbBoardRep of
                  Nothing -> initBoard rules
                  Just rep -> parseBoardRep rules rep
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
  return $ map pmMove $ possibleMoves rules currentSide board

gameState :: GameM (Side, GameStatus, Board)
gameState = do
  st <- gets gState
  status <- gets gStatus
  return (gsSide st, status, gsCurrentBoard st)

data GMoveRs = GMoveRs Board [Notify]

doMoveRq :: Side -> Move -> GameM GMoveRs
doMoveRq side move = do
  currentSide <- gets (gsSide . gState)
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  if side /= currentSide
    then throwError NotYourTurn
    else if move `notElem` (map pmMove $ possibleMoves rules side board)
           then throwError NotAllowedMove
           else do
                let (board', _, _) = applyMove rules side move board
                    moveMsg = MoveNotify (opposite side) side (moveRep rules side move) (boardRep board')
                    mbResult = getGameResult rules board'
                    messages = case mbResult of
                                 Nothing -> [moveMsg]
                                 Just result ->
                                  let resultMsg to = ResultNotify to side result
                                  in  [moveMsg, resultMsg First, resultMsg Second]
                modify $ \game -> game {gState = pushMove move board' (gState game)}
                case mbResult of
                  Just result -> 
                    modify $ \game -> game {gStatus = Ended result}
                  _ -> return ()
                return $ GMoveRs board' messages

doMoveRepRq :: Side -> MoveRep -> GameM GMoveRs
doMoveRepRq side mRep = do
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  case parseMoveRep rules side board mRep of
    NoSuchMove -> throwError NoSuchMoveError
    AmbigousMove moves -> throwError $ AmbigousMoveError $ map (moveRep rules side . pmMove) moves
    Parsed move -> doMoveRq side move

data GUndoRs = GUndoRs Board [Notify]

doUndoRq :: Side -> GameM GUndoRs
doUndoRq side = do
  currentSide <- gets (gsSide . gState)
  st <- gets gState
  if side /= currentSide
    then throwError NotYourTurn
    else case popMove st of
           Nothing -> throwError NothingToUndo
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

