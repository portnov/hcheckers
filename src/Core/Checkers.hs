{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Core.Checkers where

import Control.Monad.Reader
import qualified Control.Monad.Metrics as Metrics
import Data.Default
import Data.Maybe
import qualified Data.Text.Encoding as TE
import System.Environment
import System.Log.Heavy
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring as EKG
import Lens.Micro ((^.))

import Core.Types
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent
import Core.Rest
import Core.Config
import Core.Supervisor

import Learn
import Rules.Russian

isGameMessage :: LogMessage -> Bool
isGameMessage (LogMessage {..}) = 
  isJust $ msum $ map (lookup "game") $ map lcfVariables lmContext

withCheckers :: Checkers a -> IO a
withCheckers actions = do
  supervisor <- mkSupervisor
  cfg <- loadConfig
  print cfg
  metrics <- Metrics.initialize
  let store = metrics ^. Metrics.metricsStore
  EKG.registerGcMetrics store
  EKG.forkServerWith store (TE.encodeUtf8 $ gcHost cfg) (gcMetricsPort cfg)
  let file = (defFileSettings (gcLogFile cfg)) {
                lsFormat = "{time} [{level}] {source} [{game}|{thread}]: {message}\n"
             }
      game = Filtering isGameMessage NullLogSettings
      logSettings = ParallelLogSettings [LoggingSettings game, LoggingSettings file]
  withLoggingB logSettings $ \backend -> do
    let logger = makeLogger backend
        logging = LoggingTState logger (AnyLogBackend backend) []
        cs = CheckersState logging supervisor metrics cfg
    res <- runCheckersT actions cs
    case res of
      Right result -> return result
      Left err -> fail $ show err

