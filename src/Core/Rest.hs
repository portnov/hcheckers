{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Core.Rest where

import           Control.Monad.Reader
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception as E
import Control.Monad.Catch
import Data.String
import qualified Data.Text                     as T
import qualified Data.Text.Lazy                as TL
import           Data.Default
import           Data.Maybe
import           Data.Aeson              hiding ( json )
import           Web.Scotty.Trans
import qualified Network.HTTP.Types as H
import           Network.HTTP.Types.Status
import           Network.Wai.Handler.Warp
import qualified Network.Wai as Wai
import           System.Log.Heavy
import           System.Log.Heavy.TH
import Network.Socket
import System.Exit

import           Core.Types
import           Core.Board
import           Core.Supervisor
import           Core.Json                      ( ) -- import instances only
import           Formats.Fen
import           Formats.Pdn

type Rest a = ActionT Error Checkers a

error400 :: T.Text -> Rest ()
error400 message = do
  json $ object ["error" .= message]
  status status400

transformError :: Error -> Rest ()
transformError (Unhandled err) = do
  error400 $ T.pack err
transformError (NoSuchMoveExt move side board possible) = do
  json $ object [
      "error" .= ("no such move" :: T.Text),
      "move" .= move,
      "side" .= side,
      "board" .= board,
      "possible" .= possible
    ]
  status status400
transformError err = do
  error400 $ T.pack $ show err

raise500 :: Error -> Rest a
raise500 err = do
  text $ TL.pack $ show err
  raiseStatus status500 err

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

instance ScottyError Error where
  stringError str = Unhandled str
  showError err = TL.pack $ show err

withGameContext :: GameId -> Checkers a -> Checkers a
withGameContext gameId actions = withLogVariable "game" gameId actions

liftCheckers :: GameId -> Checkers a -> Rest a
liftCheckers gameId actions = liftCheckers' (Just gameId) actions

liftCheckers_ :: Checkers a -> Rest a
liftCheckers_ actions = liftCheckers' Nothing actions

liftCheckers' :: Maybe GameId -> Checkers a -> Rest a
liftCheckers' mbId actions = do
  res <- lift $ wrap $ tryC actions
  case res of
    Right result -> return result
    Left  err    -> raise500 err
 where
  wrap r = case mbId of
    Nothing     -> r
    Just gameId -> withGameContext gameId r

boardRq :: SupervisorState -> SomeRules -> NewGameRq -> Rest (Maybe Side, Maybe BoardRep)
boardRq _ _ (NewGameRq { rqBoard = Just br, rqFen = Nothing, rqPdn = Nothing }) =
  return $ (Nothing, Just br)
boardRq _ rules (NewGameRq { rqBoard = Nothing, rqFen = Just fen, rqPdn = Nothing })
  = case parseFen rules fen of
    Left  err        -> raise $ InvalidBoard err
    Right (side, br) -> return (Just side, Just br)
boardRq rnd rules (NewGameRq { rqBoard = Nothing, rqFen = Nothing, rqPdn = Just pdn })
  = case parsePdn (Just rules) pdn of
    Left  err -> raise $ InvalidBoard err
    Right gr  ->
      case loadPdn rnd gr of
        Left err -> raise err
        Right board -> return (Nothing, Just $ boardRep board)
boardRq _ _ (NewGameRq { rqPrevBoard = Just gameId }) = do
  board <- liftCheckers_ $ getInitialBoard gameId
  return (Nothing, Just $ boardRep board)
boardRq _ _ (NewGameRq { rqBoard = Nothing, rqFen = Nothing, rqPdn = Nothing }) =
  return (Nothing, Nothing)
boardRq _ _ _ =
  raise $ InvalidBoard "only one of fields must be filled: board, fen, pdn"

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
        (mbFirstSide, board) <- boardRq rnd rules rq
        let firstSide = fromMaybe First mbFirstSide
        gameId <- liftCheckers_ $ newGame rules firstSide board
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
    board    <- liftCheckers gameId $ doMove gameId name moveRq
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (MoveRs board) messages

  get "/game/:id/moves/:name" $ do
    gameId   <- param "id"
    name     <- param "name"
    side     <- liftCheckers gameId $ getSideByUser gameId name
    moves    <- liftCheckers gameId $ getPossibleMoves gameId side
    messages <- liftCheckers gameId $ getMessages name
    json $ Response (PossibleMovesRs moves) messages

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

restOptions :: String -> Port -> Web.Scotty.Trans.Options
restOptions host port =
  Options 0 $ setOnExceptionResponse errorHandler $
              setHost (fromString host) $
              setPort port (settings def)

errorHandler :: SomeException -> Wai.Response
errorHandler e
  | Just (err :: Error) <- fromException e =
      Wai.responseLBS H.internalServerError500
         [(H.hContentType, "text/plain; charset=utf-8")]
         (fromString $ show err)
  | otherwise =
      Wai.responseLBS H.internalServerError500
         [(H.hContentType, "text/plain; charset=utf-8")]
         (fromString $ show e)

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

ioErrorHandler :: IOError -> Checkers ()
ioErrorHandler err = liftIO $ do
  putStrLn $ "IO error: " ++ show err
  exitWith (ExitFailure 2)

runRestServer :: Checkers ()
runRestServer = do
  cs <- ask
  let getResponse m = do
        res <- runCheckersT m cs
        case res of
          Right response -> return response
          Left  err      -> fail $ show err
  host <- asks (T.unpack . gcHost . csConfig)
  port <- asks (gcPort . csConfig)
  -- portOpen <- liftIO $ checkPort host port
  shutdownVar <- liftIO newEmptyMVar
  forkCheckers $ handleIOError ioErrorHandler $ scottyOptsT (restOptions host (fromIntegral port)) getResponse (restServer shutdownVar)
  liftIO $ takeMVar shutdownVar
  -- REST thread should be able to write the response to Shutdown request.
  liftIO $ threadDelay (1000 * 1000)

