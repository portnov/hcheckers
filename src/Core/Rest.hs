{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Rest where

import Control.Monad.Reader
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Maybe
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

type Rest a = ActionT Error Checkers a

error400 :: T.Text -> Rest ()
error400 message = do
  json $ object ["error" .= message]
  status status400

transformError :: Error -> Rest ()
transformError err = do
  error400 $ T.pack $ show err

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

instance ScottyError Error where
  stringError str = Unhandled str
  showError err = TL.pack $ show err

withGameContext :: GameId -> Checkers a -> Checkers a
withGameContext gameId actions =
  withLogVariable "game" gameId actions

liftCheckers :: GameId -> Checkers a -> Rest a
liftCheckers gameId actions = liftCheckers' (Just gameId) actions

liftCheckers_ :: Checkers a -> Rest a
liftCheckers_ actions = liftCheckers' Nothing actions

liftCheckers' :: Maybe GameId -> Checkers a -> Rest a
liftCheckers' mbId actions = do
      res <- lift $ wrap $ tryC actions
      case res of
        Right result -> return result
        Left err -> raise err
  where
    wrap r =
      case mbId of
        Nothing -> r
        Just gameId -> withGameContext gameId r

boardRq :: SomeRules -> NewGameRq ->  Rest (Maybe Side, Maybe BoardRep)
boardRq _ (NewGameRq {rqBoard = Just br, rqFen = Nothing, rqPdn = Nothing}) = return $ (Nothing, Just br)
boardRq rules (NewGameRq {rqBoard = Nothing, rqFen = Just fen, rqPdn = Nothing}) =
  case parseFen rules fen of
    Left err -> raise $ InvalidBoard err
    Right (side, br) -> return (Just side, Just br)
boardRq rules (NewGameRq {rqBoard = Nothing, rqFen = Nothing, rqPdn = Just pdn}) =
  case parsePdn (Just rules) pdn of
    Left err -> raise $ InvalidBoard err
    Right gr -> return (Nothing, Just $ boardRep $ loadPdn gr)
boardRq _ (NewGameRq {rqPrevBoard = Just gameId}) = do
  board <- liftCheckers_ $ getInitialBoard gameId
  return (Nothing, Just $ boardRep board)
boardRq _ (NewGameRq {rqBoard = Nothing, rqFen = Nothing, rqPdn = Nothing}) = return (Nothing, Nothing)
boardRq _ _ = raise $ InvalidBoard "only one of fields must be filled: board, fen, pdn"

restServer :: ScottyT Error Checkers ()
restServer = do
  
  defaultHandler transformError

  post "/game/new" $ do
    rq <- jsonData
    case selectRules rq of
      Nothing -> error400 "invalid game rules"
      Just rules -> do
        (mbFirstSide, board) <- boardRq rules rq
        let firstSide = fromMaybe First mbFirstSide
        gameId <- liftCheckers_ $ newGame rules firstSide board
        liftCheckers gameId $ $info "Created new game #{}; First turn: {}; initial board: {}" (gameId, show firstSide, show board)
        json $ Response (NewGameRs gameId firstSide) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side <- param "side"
    rules <- liftCheckers gameId $ getRules gameId
    rq <- jsonData
    case selectAi rq rules of
      Nothing -> error400 "invalid ai settings"
      Just ai -> do
        liftCheckers gameId $ do
          $info "Attached AI: {} to game #{} as {}" (show ai, gameId, show side)
          initAiStorage rules ai
          attachAi gameId side ai
        json $ Response AttachAiRs []

  post "/game/:id/attach/:name/:side" $ do
    gameId <- param "id"
    name <- param "name"
    side <- param "side"
    liftCheckers gameId $ do
      registerUser gameId side name
      $info "Attached player `{}' to game #{} as {}" (name, gameId, show side)
    json $ Response RegisterUserRs []

  post "/game/:id/run" $ do
    gameId <- param "id"
    liftCheckers gameId $ runGame gameId
    json $ Response RunGameRs []

  get "/game/:id/state" $ do
    gameId <- param "id"
    rs <- liftCheckers gameId $ getState gameId
    json $ Response rs []

  get "/game/:id/fen" $ do
    gameId <- param "id"
    rs <- liftCheckers gameId $ getFen gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/pdn" $ do
    gameId <- param "id"
    rs <- liftCheckers gameId $ getPdn gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/history" $ do
    gameId <- param "id"
    rs <- liftCheckers gameId $ getHistory gameId
    json $ Response (HistoryRs rs) []

  post "/game/:id/move/:name" $ do
    gameId <- param "id"
    name <- param "name"
    moveRq <- jsonData
    board <- liftCheckers gameId $ doMove gameId name moveRq
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (MoveRs board) messages

  get "/game/:id/moves/:name" $ do
    gameId <- param "id"
    name <- param "name"
    side <- liftCheckers gameId $ getSideByUser gameId name
    moves <- liftCheckers gameId $ getPossibleMoves gameId side
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (PossibleMovesRs moves) messages

  post "/game/:id/undo/:name" $ do
    gameId <- param "id"
    name <- param "name"
    board <- liftCheckers gameId $ doUndo gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (UndoRs board) messages

  post "/game/:id/capitulate/:name" $ do
    gameId <- param "id"
    name <- param "name"
    liftCheckers gameId $ doCapitulate gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response CapitulateRs messages

  post "/game/:id/draw/request/:name" $ do
    gameId <- param "id"
    name <- param "name"
    liftCheckers gameId $ doDrawRequest gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response DrawRqRs messages

  post "/game/:id/draw/accept/:name" $ do
    gameId <- param "id"
    name <- param "name"
    liftCheckers gameId $ doDrawAccept gameId name True
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (DrawAcceptRs True) messages

  post "/game/:id/draw/decline/:name" $ do
    gameId <- param "id"
    name <- param "name"
    liftCheckers gameId $ doDrawAccept gameId name False
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (DrawAcceptRs False) messages

  get "/poll/:name" $ do
    name <- param "name"
    messages <- liftCheckers_ $ getMessages name
    json $ Response (PollRs messages) []

  get "/lobby/:rules" $ do
    rules <- param "rules"
    games <- liftCheckers_ $ getGames (Just rules)
    json $ Response (LobbyRs games) []

  get "/lobby" $ do
    games <- liftCheckers_ $ getGames Nothing
    json $ Response (LobbyRs games) []

  get "/notation/:rules" $ do
    rules <- param "rules"
    (size, orientation, notation) <- liftCheckers_ $ getNotation rules
    json $ Response (NotationRs size orientation notation) []
    
runRestServer :: Checkers ()
runRestServer = do
  cs <- ask
  let getResponse m = do
        res <- runCheckersT m cs
        case res of
          Right response -> return response
          Left err -> fail $ show err
  port <- asks (gcPort . csConfig)
  scottyT (fromIntegral port) getResponse restServer

