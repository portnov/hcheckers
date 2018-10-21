{-# LANGUAGE OverloadedStrings #-}

module Rest where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Ord
import Data.List
import Text.Printf
import Data.Aeson hiding (json)
import GHC.Generics
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Types
import Board
import Game
import Russian
import AI
import Supervisor
import Json

error400 :: T.Text -> ActionT TL.Text Checkers ()
error400 message = do
  json $ object ["error" .= message]
  status status400

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

restServer :: ScottyT TL.Text Checkers ()
restServer = do
  post "/game/new" $ do
    rq@(NewGameRq _ _ mbBoard) <- jsonData
    case selectRules rq of
      Nothing -> error400 "invalid game rules"
      Just rules -> do
        gameId <- lift $ newGame rules mbBoard
        json $ SupervisorRs (NewGameRs gameId) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side <- param "side"
    mbRules <- lift $ getRules gameId
    case mbRules of
      Nothing -> error400 "no such game"
      Just rules -> do
          rq <- jsonData
          case selectAi rq rules of
            Nothing -> error400 "invalid ai settings"
            Just ai -> do
              lift $ initAiStorage rules ai
              lift $ attachAi gameId side ai
              json $ SupervisorRs AttachAiRs []

  post "/game/:id/attach/:name/:side" $ do
    gameId <- param "id"
    name <- param "name"
    side <- param "side"
    lift $ registerUser gameId side name
    json $ SupervisorRs RegisterUserRs []

  post "/game/:id/run" $ do
    gameId <- param "id"
    lift $ runGame gameId
    json $ SupervisorRs RunGameRs []

  get "/game/:id/state" $ do
    gameId <- param "id"
    rs <- lift $ getState gameId
    json $ SupervisorRs rs []

  post "/game/:id/move/:name" $ do
    gameId <- param "id"
    name <- param "name"
    moveRq <- jsonData
    board <- lift $ doMove gameId name moveRq
    messages <- lift $ getMessages name
    json $ SupervisorRs (MoveRs board) messages

  get "/game/:id/moves/:name" $ do
    gameId <- param "id"
    name <- param "name"
    mbSide <- lift $ getSideByUser gameId name
    case mbSide of
      Nothing -> error400 "no such user in this game"
      Just side -> do
        moves <- lift $ getPossibleMoves gameId side
        messages <- lift $ getMessages name
        json $ SupervisorRs (PossibleMovesRs moves) messages

  post "/game/:id/undo/:name" $ do
    gameId <- param "id"
    name <- param "name"
    board <- lift $ doUndo gameId name
    messages <- lift $ getMessages name
    json $ SupervisorRs (UndoRs board) messages

  get "/poll/:name" $ do
    name <- param "name"
    messages <- lift $ getMessages name
    json $ SupervisorRs (PollRs messages) []

  get "/lobby/:rules" $ do
    rules <- param "rules"
    games <- lift $ getGames (Just rules)
    json $ SupervisorRs (LobbyRs games) []

  get "/lobby" $ do
    games <- lift $ getGames Nothing
    json $ SupervisorRs (LobbyRs games) []
    
runRestServer :: Checkers ()
runRestServer = do
  cs <- ask
  let getResponse m = runCheckersT m cs
  scottyT 3000 getResponse restServer

