{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rest.Game where

import Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Maybe
import           Data.Aeson              hiding ( json )
import           Web.Scotty.Trans
import           System.Log.Heavy
import           System.Log.Heavy.TH

import           Core.Types
import           Core.Board
import           Core.Supervisor
import           Core.Json                      ( ) -- import instances only
import           Formats.Types
import           Formats.Fen
import           Formats.Pdn
import Rest.Common

boardRq :: SupervisorState -> SomeRules -> NewGameRq -> Rest (Maybe Side, [HistoryRecord], Maybe BoardRep)
boardRq _ _ (NewGameRq { rqBoard = Just br, rqFen = Nothing, rqPdn = Nothing }) =
  return $ (Nothing, [], Just br)
boardRq _ rules (NewGameRq { rqBoard = Nothing, rqFen = Just fen, rqPdn = Nothing })
  = case parseFen rules fen of
    Left  err        -> raise $ InvalidBoard err
    Right (side, br) -> return (Just side, [], Just br)
boardRq rnd rules (NewGameRq { rqBoard = Nothing, rqFen = Nothing, rqPdn = Just pdn })
  = case parsePdn (Just rules) pdn of
    Left  err -> raise $ InvalidBoard err
    Right gr  ->
      case loadPdn rnd gr of
        Left err -> raise err
        Right (history, board) -> return (Nothing, history, Just $ boardRep board)
boardRq _ _ (NewGameRq { rqPrevBoard = Just gameId }) = do
  board <- liftCheckers_ $ getInitialBoard gameId
  return (Nothing, [], Just $ boardRep board)
boardRq _ _ (NewGameRq { rqBoard = Nothing, rqFen = Nothing, rqPdn = Nothing }) =
  return (Nothing, [], Nothing)
boardRq _ _ _ =
  raise $ InvalidBoard "only one of fields must be filled: board, fen, pdn"

parsePdnInfo :: PdnInfoRq -> Rest PdnInfo
parsePdnInfo (PdnInfoRq rname text) = do
  case selectRules' rname of
    Nothing -> raise UnknownRules
    Just rules ->
      case parsePdn (Just rules) text of
        Left err -> raise $ InvalidBoard err
        Right gr -> return $ pdnInfo gr

restServer :: MVar () -> ScottyT Error Checkers ()
restServer shutdownVar = do

  defaultHandler transformError

  post "/game/new" $ do
    rq <- jsonData
    case selectRules rq of
      Nothing    -> error400 "invalid game rules"
      Just rules -> do
        rnd <- liftCheckers_ $ do
                 sup <- askSupervisor
                 liftIO $ atomically $ readTVar sup
        (mbFirstSide, history, board) <- boardRq rnd rules rq
        let firstSide = fromMaybe First mbFirstSide
        gameId <- liftCheckers_ $ newGame rules firstSide board
        liftCheckers_ $ setHistory gameId history
        liftCheckers gameId $ $info
          "Created new game #{}; First turn: {}; initial board: {}"
          (gameId, show firstSide, show board)
        json $ Response (NewGameRs gameId firstSide) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side   <- param "side"
    rules  <- liftCheckers gameId $ getRules gameId
    rq     <- jsonData
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
    name   <- param "name"
    side   <- param "side"
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
    rs     <- liftCheckers gameId $ getState gameId
    json $ Response rs []

  get "/game/:id/fen" $ do
    gameId <- param "id"
    rs     <- liftCheckers gameId $ getFen gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/pdn" $ do
    gameId <- param "id"
    rs     <- liftCheckers gameId $ getPdn gameId
    Web.Scotty.Trans.text $ TL.fromStrict rs

  get "/game/:id/history" $ do
    gameId <- param "id"
    rs     <- liftCheckers gameId $ getHistory gameId
    json $ Response (HistoryRs rs) []

  post "/game/:id/move/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    moveRq   <- jsonData
    moveRs    <- liftCheckers gameId $ doMove gameId name moveRq
    messages <- liftCheckers gameId $ getMessages name
    json $ Response moveRs messages

  post "/game/:id/move/:name/:session/stop" $ do
    gameId   <- param "id"
    name     <- param "name"
    sessionId <- param "session"
    liftCheckers gameId $ signalStopAiSession sessionId
    messages <- liftCheckers gameId $ getMessages name
    json $ Response StopAiRs messages

  get "/game/:id/moves/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    side     <- liftCheckers gameId $ getSideByUser gameId name
    moves    <- liftCheckers gameId $ getPossibleMoves gameId side
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (PossibleMovesRs moves) messages

  get "/game/:id/ai/hint/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    side     <- liftCheckers gameId $ getSideByUser gameId name
    sessionId <- liftCheckers gameId $ askAiMove gameId side 
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (AiHintRs sessionId) messages

  post "/game/:id/undo/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    board    <- liftCheckers gameId $ doUndo gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (UndoRs board) messages

  post "/game/:id/capitulate/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ doCapitulate gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response CapitulateRs messages

  post "/game/:id/draw/request/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ doDrawRequest gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response DrawRqRs messages

  post "/game/:id/draw/accept/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ doDrawAccept gameId name True
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (DrawAcceptRs True) messages

  post "/game/:id/draw/decline/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ doDrawAccept gameId name False
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (DrawAcceptRs False) messages

  get "/poll/:name" $ do
    name     <- param "name"
    messages <- liftCheckers_ $ getMessages name
    json $ Response (PollRs messages) []

  get "/poll/move/:name/:id" $ do
    sessionId <- param "id"
    name     <- param "name"
    status <- liftCheckers_ $ getAiSessionStatus sessionId
    messages <- liftCheckers_ $ getMessages name
    json $ Response (PollMoveRs status) messages

  get "/lobby/:rules" $ do
    rules <- param "rules"
    games <- liftCheckers_ $ getGames (Just rules)
    json $ Response (LobbyRs games) []

  get "/lobby" $ do
    games <- liftCheckers_ $ getGames Nothing
    json $ Response (LobbyRs games) []

  get "/notation/:rules" $ do
    rules                         <- param "rules"
    (size, orientation, notation) <- liftCheckers_ $ getNotation rules
    json $ Response (NotationRs size orientation notation) []

  get "/topology/:rules" $ do
    rules <- param "rules"
    topology <- liftCheckers_ $ getTopology rules
    json $ Response (TopologyRs topology) []

  post "/file/info/pdn" $ do
    rq <- jsonData
    info <- parsePdnInfo rq
    json $ Response (PdnInfoRs info) []

  post "/server/shutdown" $ do
    isLocal <- lift $ asks (gcLocal . csConfig)
    if isLocal
      then do
        json $ Response ShutdownRs []
        liftIO $ putMVar shutdownVar ()
      else error400 "Server is not running in local mode"

  get "/status" $ do
    json $ object [
        "status" .= ("ready" :: T.Text)
      ]

-- openSocket :: AddrInfo -> IO Socket
-- openSocket addr = socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
-- 
-- checkPort :: HostName -> Port -> IO Bool
-- checkPort host port = do
--   addr <- head <$> getAddrInfo (Just defaultHints) (Just host) (Just $ show port)
--   E.bracketOnError (openSocket addr) close $ \sock -> do
--     withFdSocket sock setCloseOnExecIfNeeded
--     r <- E.try $ bind sock $ addrAddress addr
--     case r of
--       Right _ -> return True
--       Left (e :: SomeException) -> do
--         print e
--         return False

