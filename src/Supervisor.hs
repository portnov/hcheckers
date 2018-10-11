{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Data.Aeson
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
  , gRules :: SomeRules
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: [Notify]
  , gMsgbox2 :: [Notify]
  }

data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game
  }

type SupervisorHandle = TVar SupervisorState

data NewGameRq = NewGameRq String Value
  deriving (Eq, Show, Generic)

-- data RegisterUserRq = RegisterUserRq String
--   deriving (Eq, Show, Generic)

data AttachAiRq = AttachAiRq String Value
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

supportedRules :: [(String, SomeRules)]
supportedRules = [("russian", SomeRules Russian)]

selectRules :: NewGameRq -> Maybe SomeRules
selectRules (NewGameRq name params) = go supportedRules
  where
    go :: [(String, SomeRules)] -> Maybe SomeRules
    go [] = Nothing
    go ((key, (SomeRules rules)) : other)
      | key == name = Just $ SomeRules $ updateRules rules params
      | otherwise = go other

supportedAis :: [(String, SomeRules -> SomeAi)]
supportedAis = [("default", \(SomeRules rules) -> SomeAi (AlphaBeta 2 rules Russian))]

selectAi :: AttachAiRq -> SomeRules -> Maybe SomeAi
selectAi (AttachAiRq name params) rules = go supportedAis
  where
    go :: [(String, SomeRules -> SomeAi)] -> Maybe SomeAi
    go [] = Nothing
    go ((key, fn) : other)
      | key == name = Just $ updateSomeAi (fn rules) params
      | otherwise = go other

newGame :: SupervisorHandle -> SomeRules -> IO GameId
newGame var r@(SomeRules rules) = do
  handle <- spawnGame rules
  let gameId = show (gThread handle)
  let game = Game handle New r Nothing Nothing [] []
  atomically $ modifyTVar var $ \st -> st {ssGames = M.insert gameId game (ssGames st)}
  return gameId

registerUser :: SupervisorHandle -> GameId -> Side -> String -> IO ()
registerUser var gameId side name =
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update name) gameId (ssGames st)}
  where
    update name game
      | side == First = game {gPlayer1 = Just (User name)}
      | otherwise     = game {gPlayer2 = Just (User name)}

attachAi :: SupervisorHandle -> GameId -> Side -> SomeAi -> IO ()
attachAi var gameId side (SomeAi ai) = do
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
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

getRules :: SupervisorHandle -> GameId -> IO (Maybe SomeRules)
getRules var gameId = do
  mbGame <- getGame var gameId
  case mbGame of
    Nothing -> return Nothing
    Just game -> return $ Just $ gRules game

getGameByUser :: SupervisorHandle -> String -> IO (Maybe Game)
getGameByUser var name = do
  st <- atomically $ readTVar var
  case filter (\g -> isJust (sideByUser g name)) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

getMessages :: SupervisorHandle -> String -> IO [Notify]
getMessages var name = do
  atomically $ do
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

  letAiMove var game (opposite side) (Just board')

  return $ boardRep board'

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

    insert msg game
      | nDestination msg == First = game {gMsgbox1 = msg : gMsgbox1 game}
      | otherwise                 = game {gMsgbox2 = msg : gMsgbox2 game}

