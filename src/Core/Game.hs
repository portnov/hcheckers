{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

{- 
 - Game is a record for interaction between two players.
 - It is created in New status, and there are no players in the game at that moment.
 - Then two players are attached to the game (in any order). Each player can be a 
 - human user or an AI (NB: having two AI players in one game is not supported
 - currently by Supervisor).
 - After both players are attached, the game can be switched to Running state.
 -}

module Core.Game where

import Control.Monad.State
import Control.Monad.Except

import Core.Types
import Core.Board

-- | A monad to track game's state
type GameM a = ExceptT Error (State Game) a

-- | Initialize Game instance
mkGame :: GameRules rules => rules -> Int -> Side -> Maybe BoardRep -> Game
mkGame rules id firstSide mbBoardRep =
    let board = case mbBoardRep of
                  Nothing -> initBoard rules
                  Just rep -> parseBoardRep rules rep
        st = GameState firstSide board []
    in  Game {
          getGameId = show id,
          gInitialBoard = board,
          gState = st,
          gStatus = New,
          gRules = SomeRules rules,
          gPlayer1 = Nothing,
          gPlayer2 = Nothing,
          gMsgbox1 = [],
          gMsgbox2 = []
        }

-- | get currently possible moves in this game
gamePossibleMoves :: GameM [Move]
gamePossibleMoves = do
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  currentSide <- gets (gsSide . gState)
  return $ map pmMove $ possibleMoves rules currentSide board

-- | get current state of the game
gameState :: GameM (Side, GameStatus, Board)
gameState = do
  st <- gets gState
  status <- gets gStatus
  return (gsSide st, status, gsCurrentBoard st)

-- | Get game's history
gameHistory :: GameM [HistoryRecordRep]
gameHistory = do
    history <- gets (gsHistory . gState)
    some <- gets gRules
    return $ map (rep some) history
  where
    rep (SomeRules rules) r =
      let side = hrSide r
      in  HistoryRecordRep side (moveRep rules side $ hrMove r)

-- | Move result. Contains resulting board and a list of notification messages.
data GMoveRs = GMoveRs Board [Notify]

-- | Perform specified move
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

-- | Perform specified move, parsing it from MoveRep
doMoveRepRq :: Side -> MoveRep -> GameM GMoveRs
doMoveRepRq side mRep = do
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  case parseMoveRep rules side board mRep of
    NoSuchMove -> throwError NoSuchMoveError
    AmbigousMove moves -> throwError $ AmbigousMoveError $ map (moveRep rules side . pmMove) moves
    Parsed move -> doMoveRq side move

-- | Undo result
data GUndoRs = GUndoRs Board [Notify]

-- | Execute undo
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

doCapitulateRq :: Side -> GameM GameResult
doCapitulateRq side = do
  currentSide <- gets (gsSide . gState)
  if side /= currentSide
    then throwError NotYourTurn
    else do
      let result = case side of
                     First -> SecondWin
                     Second -> FirstWin
      modify $ \game -> game {gStatus = Ended result}
      return result

