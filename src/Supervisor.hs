{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}

module Supervisor where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf
import Data.IORef
import GHC.Generics

import Types
import Board
import Game
import Russian
import AI

data Player =
    User String
  | forall ai. GameAi ai => AI ai

data GameStatus = New | Running
  deriving (Eq, Show, Generic)

data Game = Game {
    gHandle :: GameHandle
  , gStatus :: GameStatus
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: [Notify]
  , gMsgbox2 :: [Notify]
  }

data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game
  }

type SupervisorHandle = TVar SupervisorState

data SupervisorRq =
    NewGameRq
  | RegisterUserRq GameId Side String
  | AttachAiRq GameId Side
  | RunGameRq GameId
  | PollRq String
  | StateRq GameId
  | PossibleMovesRq GameId Side
  | MoveRq GameId String MoveRep
  deriving (Eq, Show, Generic)

data SupervisorRs = SupervisorRs RsPayload [Notify]
  deriving (Eq, Show, Generic)

data RsPayload =
    NewGameRs GameId
  | RegisterUserRs
  | AttachAiRs
  | RunGameRs
  | PollRs [Notify]
  | StateRs BoardRep Side
  | PossibleMovesRs [MoveRep]
  | MoveRs BoardRep
  deriving (Eq, Show, Generic)

mkSupervisor :: IO SupervisorHandle
mkSupervisor = atomically $ newTVar $ SupervisorState M.empty

newGame :: SupervisorHandle -> IO GameId
newGame var = do
  handle <- spawnGame Russian
  let gameId = show (gThread handle)
  let game = Game handle New Nothing Nothing [] []
  atomically $ modifyTVar var $ \st -> st {ssGames = M.insert gameId game (ssGames st)}
  return gameId

registerUser :: SupervisorHandle -> GameId -> Side -> String -> IO ()
registerUser var gameId side name =
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update name) gameId (ssGames st)}
  where
    update name game
      | side == First = game {gPlayer1 = Just (User name)}
      | otherwise     = game {gPlayer2 = Just (User name)}

attachAi :: SupervisorHandle -> GameId -> Side -> IO ()
attachAi var gameId side = do
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
    ai = AlphaBeta 2 Russian Russian

    update game
      | side == First = game {gPlayer1 = Just $ AI ai}
      | otherwise     = game {gPlayer2 = Just $ AI ai}

runGame :: SupervisorHandle -> GameId -> IO ()
runGame var gameId = do
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    mbGame <- getGame var gameId
    let game = fromJust mbGame
    letAiMove var game First Nothing
    return ()
  where
    update game = game {gStatus = Running}

getGame :: SupervisorHandle -> GameId -> IO (Maybe Game)
getGame var gameId = do
  st <- atomically $ readTVar var
  return $ M.lookup gameId (ssGames st)

getMessages :: SupervisorHandle -> String -> IO [Notify]
getMessages var name = atomically $ do
    st <- readTVar var
    let messages = concatMap getM (M.elems $ ssGames st)
    let st' = st {ssGames = M.map updateGame (ssGames st)}
    writeTVar var st'
    return messages
  where
    updateGame game =
      case sideByUser game name of
        Nothing -> game
        Just First -> game {gMsgbox1 = []}
        Just Second -> game {gMsgbox2 = []}

    getM game = 
      case sideByUser game name of
        Nothing -> []
        Just First -> gMsgbox1 game
        Just Second -> gMsgbox2 game
    

doMove :: SupervisorHandle -> GameId -> String -> MoveRep -> IO BoardRep
doMove var gameId name moveRq = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  let input = gInput $ gHandle game
  let output = gOutput $ gHandle game
  let side = fromJust $ sideByUser game name
  writeChan input $ DoMoveRepRq side moveRq
  DoMoveRs board' messages <- readChan output
  queueNotifications var gameId messages

  board'' <- letAiMove var game (opposite side) (Just board')

  return $ boardRep board''

letAiMove :: SupervisorHandle -> Game -> Side -> Maybe Board -> IO Board
letAiMove var game side mbBoard = do
  board <- case mbBoard of
             Just b -> return b
             Nothing -> do
               writeChan (gInput $ gHandle game) GStateRq
               GStateRs _ b <- readChan (gOutput $ gHandle game)
               return b

  case getPlayer game side of
    AI ai -> do
      aiMove <- chooseMove ai side board
      putStrLn $ "AI move: " ++ show aiMove
      writeChan (gInput $ gHandle game) $ DoMoveRq side aiMove
      DoMoveRs board' messages <- readChan (gOutput $ gHandle game)
      putStrLn $ "Messages: " ++ show messages
      queueNotifications var (getGameId $ gHandle game) messages
      return board'

    _ -> return board

getState :: SupervisorHandle -> GameId -> IO RsPayload
getState var gameId = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  writeChan (gInput $ gHandle game) GStateRq
  GStateRs side board <- readChan (gOutput $ gHandle game)
  return $ StateRs (boardRep board) side

getPlayer :: Game -> Side -> Player
getPlayer game First = fromJust $ gPlayer1 game
getPlayer game Second = fromJust $ gPlayer2 game

isAI :: Game -> Side -> Bool
isAI game First = case gPlayer1 game of
                    Just (AI _) -> True
                    _ -> False
isAI game Second = case gPlayer2 game of
                     Just (AI _) -> True
                     _ -> False

sideByUser :: Game -> String -> Maybe Side
sideByUser game name =
  case (gPlayer1 game, gPlayer2 game) of
    (Just (User name1), _) | name1 == name -> Just First
    (_, Just (User name2)) | name2 == name -> Just Second
    _ -> Nothing

queueNotifications :: SupervisorHandle -> GameId -> [Notify] -> IO ()
queueNotifications var gameId messages = atomically $ modifyTVar var go
  where
    go st = foldl route st messages

    route st msg = st {ssGames = M.update (Just . insert msg) gameId (ssGames st)}

    insert msg@(Notify side _ _) game
      | side == First = game {gMsgbox1 = msg : gMsgbox1 game}
      | otherwise     = game {gMsgbox1 = msg : gMsgbox2 game}

