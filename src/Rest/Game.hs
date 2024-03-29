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
import Network.HTTP.Types.Status

import Core.Types
import Core.Board
import Core.Supervisor
import Core.Version
import Core.Json                      ( ) -- import instances only
import AI.Session
import Formats.Types
import Formats.Fen
import Formats.Pdn
import Rest.Types
import Rest.Common

boardRq :: SupervisorState -> SomeRules -> NewGameRq -> Rest (Maybe Side, [HistoryRecord], Maybe BoardRep)
boardRq _ _ (NewGameRq { rqBoard = ExplicitBoard br }) =
  return $ (Nothing, [], Just br)
boardRq _ rules (NewGameRq { rqBoard = FenBoard fen }) =
  case parseFen rules fen of
    Left  err        -> raise $ InvalidBoard err
    Right (side, br) -> return (Just side, [], Just br)
boardRq rnd rules (NewGameRq { rqBoard = PdnBoard pdn }) =
  case parsePdn (Just rules) pdn of
    Left  err -> raise $ InvalidBoard err
    Right gr  ->
      case loadPdn rnd gr of
        Left err -> raise err
        Right (history, board) -> return (Nothing, history, Just $ boardRep board)
boardRq _ rules (NewGameRq { rqBoard = RandomPreset }) = do
  board <- liftCheckers_ $ getRandomInitialBoard  rules
  return (Nothing, [], board)
boardRq _ _ (NewGameRq { rqBoard = PrevGameBoard gameId }) = do
  board <- liftCheckers_ $ getInitialBoard gameId
  return (Nothing, [], Just $ boardRep board)
boardRq _ _ (NewGameRq { rqBoard = DefaultBoard }) =
  return (Nothing, [], Nothing)

parsePdnInfo :: PdnInfoRq -> Rest PdnInfo
parsePdnInfo (PdnInfoRq rname text) = do
  case selectRulesEither' rname of
    Left err -> raise $ UnknownRules err
    Right rules ->
      case parsePdn (Just rules) text of
        Left err -> raise $ InvalidBoard err
        Right gr -> return $ pdnInfo gr

maybeParam :: forall a. Parsable a => TL.Text -> Rest (Maybe a)
maybeParam name =
  (Just <$> param name) `rescue` (\_ -> return (Nothing :: Maybe a))

restServer :: MVar () -> ScottyT Error Checkers ()
restServer shutdownVar = do

  defaultHandler transformError

  get "/version" $ do
    json getVersion

  post "/game/new" $ do
    rq <- jsonData
    case selectRulesEither rq of
      Left err    -> raise $ UnknownRules err
      Right rules -> do
        rnd <- liftCheckers_ $ do
                 sup <- askSupervisor
                 liftIO $ atomically $ readTVar sup
        (mbFirstSide, history, board) <- boardRq rnd rules rq
        let firstSide = fromMaybe First mbFirstSide
        gameId <- liftCheckers_ $ newGame rules firstSide board (rqTimingControl rq)
        liftCheckers_ $ setHistory gameId history
        liftCheckers gameId $ $info
          "Created new game #{}; First turn: {}; Timing control: {}; initial board: {}"
          (gameId, show firstSide, rqTimingControl rq, show board)
        json $ Response (NewGameRs gameId firstSide) []

  post "/game/:id/attach/ai/:side" $ do
    gameId <- param "id"
    side   <- param "side"
    rules  <- liftCheckers gameId $ getRules gameId
    rq     <- jsonData
    mbAi <- liftCheckers_ $ selectAi rq rules 
    case mbAi of
      Nothing -> error400 "invalid ai settings"
      Just (ai, name) -> do
        liftCheckers gameId $ do
          $info "Attached AI: {} as {} to game #{} as {}" (show ai, name, gameId, show side)
          initAiStorage rules ai
          attachAi gameId side name ai
        json $ Response AttachAiRs []

  post "/game/:id/attach/:name/spectate" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ do
      attachSpectator gameId name
      $info "Attached spectator `{}' to game #{}" (name, gameId)
    json $ Response AttachSpectatorRs []

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

  post "/game/:id/run/loop" $ do
    gameId <- param "id"
    liftCheckers gameId $ runGameLoop gameId
    json $ Response RunGameLoopRs []

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

  get "/game/:id/history/:turn/:side" $ do
    gameId <- param "id"
    turnIdx <- param "turn"
    side <- param "side"
    (move, prevBoard, nextBoard) <- liftCheckers gameId $ getMoveWithResult gameId turnIdx side
    json $ Response (HistoryMoveRs move prevBoard nextBoard) []

  get "/game/:id/history/init/board" $ do
    gameId <- param "id"
    rs <- liftCheckers gameId $ getInitBoard gameId
    json $ Response (HistoryBoardRs rs) []

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
    (sessionId, hintCount) <- liftCheckers gameId $ askAiMove gameId side 
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (AiHintRs hintCount sessionId) messages

  post "/game/:id/undo/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    (board, undoCount) <- liftCheckers gameId $ doUndo gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (UndoRs undoCount board) messages

  post "/game/:id/capitulate/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    liftCheckers gameId $ doCapitulate gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response CapitulateRs messages

  post "/game/:id/draw/request/:name" $ do
    gameId <- param "id"
    name   <- param "name"
    sessionId <- liftCheckers gameId $ doDrawRequest gameId name
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (DrawRqRs sessionId) messages

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

  get "/ai/custom/permissions" $ do
    enabled <- liftCheckers_ isCustomAiSettingsEnabled
    json $ object ["custom_settings_enabled" .= enabled]

  get "/ai/:impl" $ do
    impl <- param "impl"
    personalities <- liftCheckers_ $ listAiSettings impl
    json (AiPersonalities personalities)

  get "/ai/:impl/:slug" $ do
    impl <- param "impl"
    slug <- param "slug"
    mbPersonality <- liftCheckers_ $ loadAiSetting impl slug
    case mbPersonality of
      Just personality -> json personality
      Nothing -> status status404

  get "/timing" $ do
    mbRules <- maybeParam "rules"
    options <- liftCheckers_ $ getTimingOptions mbRules
    json options

  get "/timing/:slug" $ do
    slug <- param "slug"
    config <- liftCheckers_ $ getTimingConfig slug
    json config

  get "/poll/:name" $ do
    name     <- param "name"
    messages <- liftCheckers_ $ getMessages name
    mbGame <- liftCheckers_ $ getGameByUser' name Running
    timeMessages <- case mbGame of
                      Nothing -> return []
                      Just game -> do
                        let gameId = getGameId game
                        mbTiming <- liftCheckers gameId $ getTiming gameId
                        case mbTiming of
                          Just (firstLeft, secondLeft) -> do
                            side <- liftCheckers gameId $ getSideByUser gameId name
                            return [TimingNotify side firstLeft secondLeft]
                          Nothing -> return []
    let messages' = messages ++ timeMessages
    json $ Response (PollRs messages') []

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
    rules <- param "rules"
    rs <- liftCheckers_ $ getNotation rules
    json $ Response rs []

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

