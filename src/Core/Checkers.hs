{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Core.Checkers where

import Control.Monad (when)
import Control.Concurrent
import qualified Control.Monad.Metrics as Metrics
import Data.Maybe
import qualified Data.Text.Encoding as TE
import System.Log.Heavy
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring as EKG
import Lens.Micro ((^.))

import Core.Types
import Core.CmdLine
import Core.Config
import Core.Supervisor

isGameMessage :: LogMessage -> Bool
isGameMessage msg = 
  isJust $ gameIdFromLogMsg msg

withCheckers :: CmdLine -> Checkers a -> IO a
withCheckers cmd actions = do
  supervisor <- mkSupervisor
  cfg <- loadConfig cmd
  logChan <- newChan
  metrics <- Metrics.initialize
  when (gcEnableMetrics cfg) $ do
    let store = metrics ^. Metrics.metricsStore
    EKG.registerGcMetrics store
    EKG.forkServerWith store (TE.encodeUtf8 $ gcHost cfg) (gcMetricsPort cfg)
    return ()
  let file = (defFileSettings (gcLogFile cfg)) {
                lsFormat = "{time} [{level}] {source} [{game}|{thread}]: {message}\n"
             }
      game = Filtering isGameMessage $ ChanLoggerSettings logChan
      logSettings = ParallelLogSettings [LoggingSettings game, LoggingSettings file]
  withLoggingB logSettings $ \backend -> do
    let logger = makeLogger backend
        logging = LoggingTState logger (AnyLogBackend backend) []
        cs = CheckersState logging supervisor metrics cfg
        actions' = do
          forkCheckers $ logRouter supervisor logChan
          actions
    res <- runCheckersT actions' cs
    case res of
      Right result -> return result
      Left err -> fail $ show err

