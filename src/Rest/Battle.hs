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

import Core.Types
import Core.Json ( ) -- import instances only
import Core.Supervisor (selectRules')

import Battle (runBattleLocal)
import AI (parseAi)
import Rest.Common

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

