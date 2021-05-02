{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Rest.Common where

import Control.Monad.Reader
import Control.Concurrent
import Control.Exception as E
import Control.Monad.Catch
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Default
import Data.Aeson hiding (json)
import Data.String
import System.Log.Heavy
import Web.Scotty.Trans
import Network.HTTP.Types.Status
import qualified Network.HTTP.Types as H
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp
import System.Exit

import Core.Types
import Core.Json () -- import instances only

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

raise500 :: Error -> Rest a
raise500 err = do
  text $ TL.pack $ show err
  raiseStatus status500 err

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
transformError (InvalidGameStatus expected actual) = do
  json $ object [
      "error" .= ("invalid game status" :: T.Text),
      "expected" .= expected,
      "actual" .= actual
    ]
transformError err = do
  error400 $ T.pack $ show err

instance Parsable Side where
  parseParam "1" = Right First
  parseParam "2" = Right Second
  parseParam text = Left $ "unknown side"

instance ScottyError Error where
  stringError str = Unhandled str
  showError err = TL.pack $ show err

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

ioErrorHandler :: IOError -> Checkers ()
ioErrorHandler err = liftIO $ do
  putStrLn $ "IO error: " ++ show err
  exitWith (ExitFailure 2)

runRestServer :: String -> Int -> (MVar () -> ScottyT Error Checkers ()) -> Checkers ()
runRestServer host port restServer = do
  cs <- ask
  let getResponse m = do
        res <- runCheckersT m cs
        case res of
          Right response -> return response
          Left  err      -> fail $ show err
  -- portOpen <- liftIO $ checkPort host port
  shutdownVar <- liftIO newEmptyMVar
  forkCheckers $ handleIOError ioErrorHandler $ scottyOptsT (restOptions host (fromIntegral port)) getResponse (restServer shutdownVar)
  liftIO $ takeMVar shutdownVar
  -- REST thread should be able to write the response to Shutdown request.
  liftIO $ threadDelay (1000 * 1000)

data BattleRq = BattleRq {
      brqRules :: String
    , brqAi1 :: Value
    , brqAi2 :: Value
    }

instance ToJSON BattleRq where
  toJSON rq =
    object ["rules" .= brqRules rq, "ai1" .= brqAi1 rq, "ai2" .= brqAi2 rq]

instance FromJSON BattleRq where
  parseJSON (Object v) = BattleRq
    <$> v .: "rules"
    <*> v .: "ai1"
    <*> v .: "ai2"

