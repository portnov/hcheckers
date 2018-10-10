{-# LANGUAGE DeriveGeneric #-}

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

data Player =
    User String
  | AI
  deriving (Eq, Show, Generic)

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
  | MoveRq GameId MoveRep
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
    update game
      | side == First = game {gPlayer1 = Just AI}
      | otherwise     = game {gPlayer2 = Just AI}

runGame :: SupervisorHandle -> GameId -> IO ()
runGame var gameId = 
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
    update game = game {gStatus = Running}

queueNotifications :: SupervisorHandle -> GameId -> [Notify] -> IO ()
queueNotifications var gameId messages = atomically $ modifyTVar var go
  where
    go st = foldl route st messages

    route st msg = st {ssGames = M.update (Just . insert msg) gameId (ssGames st)}

    insert msg@(Notify side _ _) game
      | side == First = game {gMsgbox1 = msg : gMsgbox1 game}
      | otherwise     = game {gMsgbox1 = msg : gMsgbox2 game}

