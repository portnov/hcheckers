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
import Control.Concurrent.STM
import qualified Data.Map as M
import Data.Default

import Core.Types
import Core.Board
import qualified Core.Rope as R

-- | Initialize Game instance
mkGame :: GameRules rules => SupervisorState -> rules -> Int -> Side -> Maybe BoardRep -> Maybe TimingConfig -> STM Game
mkGame supervisor rules id firstSide mbBoardRep mbTcfg = do
    let board = case mbBoardRep of
                  Nothing -> initBoard supervisor rules
                  Just rep -> parseBoardRep supervisor rules rep
        st = GameState firstSide board []
    msgbox1 <- newTChan
    msgbox2 <- newTChan
    return $ Game {
          getGameId = show id,
          gInitialBoard = board,
          gState = st,
          gStatus = New,
          gRules = SomeRules rules,
          gPlayer1 = Nothing,
          gPlayer2 = Nothing,
          gMsgbox1 = msgbox1,
          gMsgbox2 = msgbox2,
          gStats1 = def,
          gStats2 = def,
          gTimingConfig = mbTcfg,
          gTiming1 = def,
          gTiming2 = def,
          gSpectatorsMsgBox = M.empty
        }

-- | Get game by Id
getGame :: GameId -> Checkers Game
getGame gameId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case M.lookup gameId (ssGames st) of
    Just game -> return game
    Nothing -> throwError $ NoSuchGame gameId

-- | Check current game status. 
-- Throw an error if it is not as expected.
-- Do nothing otherwise.
checkStatus :: GameStatus -> GameM ()
checkStatus expected = do
  status <- gets gStatus
  when (status /= expected) $
    throwError $ InvalidGameStatus expected status

checkCurrentSide :: Side -> GameM ()
checkCurrentSide side = do
  currentSide <- gets (gsSide . gState)
  when (side /= currentSide) $
    throwError NotYourTurn

-- | get currently possible moves in this game
gamePossibleMoves :: GameM [Move]
gamePossibleMoves = do
  -- checkStatus Running
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  currentSide <- gets (gsSide . gState)
  return $ R.toList $ fmap pmMove $ possibleMoves rules currentSide board

-- | get current state of the game
gameState :: GameM (Side, GameStatus, Board)
gameState = do
  st <- gets gState
  status <- gets gStatus
  return (gsSide st, status, gsCurrentBoard st)

gameBoard :: GameM Board
gameBoard =
  gets (gsCurrentBoard . gState)

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

moveWithResult :: Int -> Side -> GameM (MoveRep, BoardRep, BoardRep)
moveWithResult i side = do
  history <- gets (gsHistory . gState)
  let nMoves = length history
  let idxDelta = fromEnum $ hrSide (last history)
  let moveIdx = i*2 + fromEnum side -- - idxDelta
  when (moveIdx < 0) $
    throwError $ Unhandled "Turn number can not be negative"
  when (moveIdx >= nMoves) $
    throwError $ Unhandled "Turn number is too big"
  let moveIdx' = nMoves - moveIdx

  SomeRules rules <- gets gRules
  let moveRep' hr = moveRep rules (hrSide hr) (hrMove hr)

  if moveIdx == nMoves-1
      then do
        resultBoard <- gets (gsCurrentBoard . gState)
        let record = head history
        return (moveRep' record, boardRep (hrPrevBoard record), boardRep resultBoard)
      else do
        let nextMoveIdx = moveIdx' - 1
        let record = history  !! nextMoveIdx
        let nextRecord = history !! (nextMoveIdx - 1)
        return (moveRep' record, boardRep $ hrPrevBoard record, boardRep $ hrPrevBoard nextRecord)

gameInitBoard :: GameM BoardRep
gameInitBoard = do
  history <- gets (gsHistory . gState)
  return $ boardRep $ hrPrevBoard $ last history

setGameHistory :: [HistoryRecord] -> GameM ()
setGameHistory history = do
  checkStatus New
  modify $ \st -> st {
              gState = (gState st) {gsHistory = history}
            }

-- | Number of half-moves done in this game
gameMoveNumber :: Game -> Int
gameMoveNumber g =
  let history = gsHistory $ gState g
  in  length history

-- | Move result. Contains resulting board and a list of notification messages.
data GMoveRs = GMoveRs Board [Notify]

setGameResult :: GameResult -> GameM ()
setGameResult result =
  modify $ \game -> game {gStatus = Ended result}

-- | Perform specified move
doMoveRq :: Side -> Move -> GameM GMoveRs
doMoveRq side move = do
  checkStatus Running
  checkCurrentSide side
  SomeRules rules <- gets gRules
  board <- gets (gsCurrentBoard . gState)
  if move `R.notElem` (fmap pmMove $ possibleMoves rules side board)
     then throwError NotAllowedMove
     else do
          st <- gets gState
          let (board', _, _) = applyMove rules side move board
              moveMsg = MoveNotify (opposite side) side (moveRep rules side move) (boardRep board')
              mbResult = getGameResult rules st board' (opposite side)
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
data GUndoRs = GUndoRs Board Int [Notify]

setUndoCount :: Int -> GamePlayerStats -> GamePlayerStats
setUndoCount n st = st {pUndoCount = n}

setHintCount :: Int -> GamePlayerStats -> GamePlayerStats
setHintCount n st = st {pAiHintsCount = n}

increaseUndoCount :: Side -> Game -> Game
increaseUndoCount First  game = game {gStats1 = setUndoCount (pUndoCount (gStats1 game) + 1) (gStats1 game)}
increaseUndoCount Second game = game {gStats2 = setUndoCount (pUndoCount (gStats2 game) + 1) (gStats2 game)}

increaseHintCount :: Side -> Game -> Game
increaseHintCount First  game = game {gStats1 = setHintCount (pAiHintsCount (gStats1 game) + 1) (gStats1 game)}
increaseHintCount Second game = game {gStats2 = setHintCount (pAiHintsCount (gStats2 game) + 1) (gStats2 game)}

getUndoCount :: Side -> Game -> Int
getUndoCount First  g = pUndoCount (gStats1 g)
getUndoCount Second g = pUndoCount (gStats2 g)

getHintCount :: Side -> Game -> Int
getHintCount First  g = pAiHintsCount (gStats1 g)
getHintCount Second g = pAiHintsCount (gStats2 g)

-- | Execute undo
doUndoRq :: Side -> GameM GUndoRs
doUndoRq side = do
  checkStatus Running
  checkCurrentSide side
  st <- gets gState
  case popMove st of
     Nothing -> throwError NothingToUndo
     Just (prevBoard, prevSt) -> do
       let push = UndoNotify (opposite side) side (boardRep prevBoard)
       modify $ \game -> game {gState = prevSt}
       modify $ increaseUndoCount side
       undoCount <- gets (getUndoCount side)
       return $ GUndoRs prevBoard undoCount [push]

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
  checkStatus Running
  checkCurrentSide side
  let result = case side of
                 First -> SecondWin
                 Second -> FirstWin
  modify $ \game -> game {gStatus = Ended result}
  return result

doPostDrawRequest :: Side -> GameM ()
doPostDrawRequest side = do
  checkStatus Running
  checkCurrentSide side
  modify $ \game -> game {gStatus = DrawRequested side}

doDrawAcceptRq :: Side -> Bool -> GameM ()
doDrawAcceptRq side accepted = do
  checkStatus (DrawRequested (opposite side))
  if accepted
    then modify $ \game -> game {gStatus = Ended Draw}
    else modify $ \game -> game {gStatus = Running}

