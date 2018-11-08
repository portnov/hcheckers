{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Rest where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Aeson hiding (json)
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.Supervisor
import Core.Json () -- import instances only
import Formats.Fen
import Formats.Pdn

error400 :: T.Text -> ActionT Error Checkers ()
error400 message = do
  json $ object ["error" .= message]
  status status400

transformError :: Error -> ActionT Error Checkers ()
transformError err = do
  error400 $ T.pack $ show err

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

instance ScottyError Error where
  stringError str = Unhandled str
  showError err = TL.pack $ show err

liftCheckers :: Checkers a -> ActionT Error Checkers a
liftCheckers actions = do
  res <- lift $ tryC actions
  case res of
    Right result -> return result
    Left err -> raise err

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

restServer :: ScottyT Error Checkers ()
restServer = do
  
  defaultHandler transformError

  post "/game/new" $ do
    rq@(NewGameRq {rqBoard=mbBoard, rqFen=mbFen, rqPdn=mbPdn}) <- jsonData
    case selectRules rq of
      Nothing -> error400 "invalid game rules"
      Just rules -> do
        case boardRq rules mbBoard mbFen mbPdn of
          Left err -> error400 err
          Right board -> do
            gameId <- liftCheckers $ newGame rules board
            liftCheckers $ $info "Created new game #{} with board: {}" (gameId, show board)
            json $ SupervisorRs (NewGameRs gameId) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side <- param "side"
    rules <- liftCheckers $ getRules gameId
    rq <- jsonData
    case selectAi rq rules of
      Nothing -> error400 "invalid ai settings"
      Just ai -> do
        liftCheckers $ $info "Attached AI: {} to game #{}" (show ai, gameId)
        liftCheckers $ initAiStorage rules ai
        liftCheckers $ attachAi gameId side ai
        json $ SupervisorRs AttachAiRs []

  post "/game/:id/attach/:name/:side" $ do
    gameId <- param "id"
    name <- param "name"
    side <- param "side"
    liftCheckers $ registerUser gameId side name
    json $ SupervisorRs RegisterUserRs []

  post "/game/:id/run" $ do
    gameId <- param "id"
    liftCheckers $ runGame gameId
    json $ SupervisorRs RunGameRs []

  get "/game/:id/state" $ do
    gameId <- param "id"
    rs <- liftCheckers $ getState gameId
    json $ SupervisorRs rs []

  get "/game/:id/fen" $ do
    gameId <- param "id"
    rs <- liftCheckers $ getFen gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/pdn" $ do
    gameId <- param "id"
    rs <- liftCheckers $ getPdn gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  post "/game/:id/move/:name" $ do
    gameId <- param "id"
    name <- param "name"
    moveRq <- jsonData
    board <- liftCheckers $ doMove gameId name moveRq
    messages <- liftCheckers $ getMessages name
    json $ SupervisorRs (MoveRs board) messages

  get "/game/:id/moves/:name" $ do
    gameId <- param "id"
    name <- param "name"
    side <- liftCheckers $ getSideByUser gameId name
    moves <- liftCheckers $ getPossibleMoves gameId side
    messages <- liftCheckers $ getMessages name
    json $ SupervisorRs (PossibleMovesRs moves) messages

  post "/game/:id/undo/:name" $ do
    gameId <- param "id"
    name <- param "name"
    board <- liftCheckers $ doUndo gameId name
    messages <- liftCheckers $ getMessages name
    json $ SupervisorRs (UndoRs board) messages

  get "/poll/:name" $ do
    name <- param "name"
    messages <- liftCheckers $ getMessages name
    json $ SupervisorRs (PollRs messages) []

  get "/lobby/:rules" $ do
    rules <- param "rules"
    games <- liftCheckers $ getGames (Just rules)
    json $ SupervisorRs (LobbyRs games) []

  get "/lobby" $ do
    games <- liftCheckers $ getGames Nothing
    json $ SupervisorRs (LobbyRs games) []

  get "/notation/:rules" $ do
    rules <- param "rules"
    (size, notation) <- liftCheckers $ getNotation rules
    json $ SupervisorRs (NotationRs size notation) []
    
runRestServer :: Checkers ()
runRestServer = do
  cs <- ask
  let getResponse m = do
        res <- runCheckersT m cs
        case res of
          Right response -> return response
          Left err -> fail $ show err
  scottyT 3000 getResponse restServer

