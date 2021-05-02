{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

module Rest.Battle where

import Control.Monad.Reader
import Control.Concurrent
import qualified Data.Text as T
import Data.Aeson hiding ( json )
import Web.Scotty.Trans
import Network.HTTP.Req
import Text.URI (mkURI)

import Core.Types
import Core.Json ( ) -- import instances only
import Core.Supervisor (selectRules')

import Battle (BattleRunner, runBattleLocal)
import AI (parseAi)
import Rest.Common

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

withRulesR :: String -> (forall rules. GameRules rules => rules -> Rest a) -> Rest a
withRulesR rname fn =
  case selectRules' rname of
    Just (SomeRules rules) -> fn rules
    Nothing -> raise UnknownRules

restServer :: MVar () -> ScottyT Error Checkers ()
restServer shutdownVar = do
  
  post "/battle/run" $ do
    rq <- jsonData
    withRulesR (brqRules rq) $ \rules -> do
      let ab1 = parseAi rules (brqAi1 rq)
          ab2 = parseAi rules (brqAi2 rq)
          ai1 = SomeAi ab1
          ai2 = SomeAi ab2
      result <- liftCheckers_ $ runBattleLocal (SomeRules rules) (1,ai1) (2,ai2) "battle.pdn"
      json result

  post "/server/shutdown" $ do
    isLocal <- lift $ asks (gcLocal . csConfig)
    if isLocal
      then do
        json $ object ["shutdown" .= ("ok" :: T.Text)]
        liftIO $ putMVar shutdownVar ()
      else error400 "Server is not running in local mode"

decodeFile :: FromJSON a => FilePath -> IO a
decodeFile path = do
  r <- eitherDecodeFileStrict' path
  case r of
    Left err -> fail err
    Right x -> return x

http_ :: T.Text -> (Url 'Http, Option s)
http_ text =
  case mkURI text of
    Nothing -> error $ "Can't parse URI: " ++ T.unpack text
    Just uri ->
      case useHttpURI uri of
        Nothing -> error $ "Unsupported URI: " ++ T.unpack text
        Just (url, opts) -> (url, opts)

runBattleRemoteIO :: T.Text -> String -> FilePath -> FilePath -> IO GameResult
runBattleRemoteIO baseUrl rulesName aiPath1 aiPath2 = do
  ai1 <- decodeFile aiPath1
  ai2 <- decodeFile aiPath2
  let rq = BattleRq rulesName ai1 ai2
      (url, opts) = http_ baseUrl 
  rs <- runReq defaultHttpConfig $
          req POST (url /: "battle" /: "run") (ReqBodyJson rq) jsonResponse opts
  return (responseBody rs)

runBattleRemote :: T.Text -> BattleRunner
runBattleRemote baseUrl (SomeRules rules) (i,SomeAi ai1) (j,SomeAi ai2) path = do
  let rq = BattleRq (rulesName rules) (toJSON ai1) (toJSON ai2)
      (url, opts) = http_ baseUrl
  rs <- liftIO $ runReq defaultHttpConfig $
          req POST (url /: "battle" /: "run") (ReqBodyJson rq) jsonResponse opts
  return (responseBody rs)

