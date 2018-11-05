{-# LANGUAGE OverloadedStrings #-}

module Core.Rest where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson hiding (json)
import Web.Scotty.Trans
import Network.HTTP.Types.Status

import Core.Types
import Core.Board
import Core.Supervisor
import Core.Json () -- import instances only
import Formats.Fen
import Formats.Pdn

error400 :: T.Text -> ActionT TL.Text Checkers ()
error400 message = do
  json $ object ["error" .= message]
  status status400

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

boardRq :: SomeRules -> Maybe BoardRep -> Maybe T.Text -> Maybe T.Text -> Either T.Text (Maybe BoardRep)
boardRq _ (Just br) Nothing Nothing = Right $ Just br
boardRq rules Nothing (Just fen) Nothing =
  case parseFen rules fen of
    Left err -> Left $ T.pack err
    Right br -> Right $ Just br
boardRq rules Nothing Nothing (Just pdn) =
  case parsePdn pdn of
    Left err -> Left $ T.pack err
    Right gr -> Right $ Just $ boardRep $ loadPdn gr
boardRq _ Nothing Nothing Nothing = Right Nothing
boardRq _ _ _ _ = Left "only one of fields must be filled: board, fen, pdn"

restServer :: ScottyT TL.Text Checkers ()
restServer = do
  post "/game/new" $ do
    rq@(NewGameRq {rqBoard=mbBoard, rqFen=mbFen, rqPdn=mbPdn}) <- jsonData
    case selectRules rq of
      Nothing -> error400 "invalid game rules"
      Just rules -> do
        case boardRq rules mbBoard mbFen mbPdn of
          Left err -> error400 err
          Right board -> do
            liftIO $ print board
            gameId <- lift $ newGame rules board
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
              liftIO $ putStrLn $ "Attached AI: " ++ show ai
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

  get "/game/:id/fen" $ do
    gameId <- param "id"
    rs <- lift $ getFen gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/pdn" $ do
    gameId <- param "id"
    rs <- lift $ getPdn gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

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

  get "/notation/:rules" $ do
    rules <- param "rules"
    (size, notation) <- lift $ getNotation rules
    json $ SupervisorRs (NotationRs size notation) []
    
runRestServer :: Checkers ()
runRestServer = do
  cs <- ask
  let getResponse m = runCheckersT m cs
  scottyT 3000 getResponse restServer

