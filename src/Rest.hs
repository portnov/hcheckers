{-# LANGUAGE OverloadedStrings #-}

module Rest where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Ord
import Data.List
import Text.Printf
import Data.IORef
import GHC.Generics
import Web.Scotty

import Types
import Board
import Game
import Russian
import AI
import Supervisor
import Json

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

restServer :: SupervisorHandle -> ScottyM ()
restServer supervisor = do
  post "/game/new" $ do
    gameId <- liftIO $ newGame supervisor
    json $ SupervisorRs (NewGameRs gameId) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side <- param "side"
    liftIO $ attachAi supervisor gameId side
    json $ SupervisorRs AttachAiRs []

  post "/game/:id/attach/:name/:side" $ do
    gameId <- param "id"
    name <- param "name"
    side <- param "side"
    liftIO $ registerUser supervisor gameId side name
    json $ SupervisorRs RegisterUserRs []

  post "/game/:id/run" $ do
    gameId <- param "id"
    liftIO $ runGame supervisor gameId
    json $ SupervisorRs RunGameRs []

  get "/game/:id/state" $ do
    gameId <- param "id"
    rs <- liftIO $ getState supervisor gameId
    json $ SupervisorRs rs []

  post "/game/:id/move/:name" $ do
    gameId <- param "id"
    name <- param "name"
    moveRq <- jsonData
    board <- liftIO $ doMove supervisor gameId name moveRq
    messages <- liftIO $ getMessages supervisor name
    json $ SupervisorRs (MoveRs board) messages

  get "/poll/:name" $ do
    name <- param "name"
    messages <- liftIO $ getMessages supervisor name
    json $ SupervisorRs (PollRs messages) []
    
runRestServer :: IO ()
runRestServer = do
  supervisor <- mkSupervisor
  scotty 3000 (restServer supervisor)

