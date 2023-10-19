{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import System.Log.Heavy
import Options.Applicative

import Core.Types hiding (timed)
import qualified Core.Version as V
import Rest.Common (runRestServer)
import Rest.Game (restServer)
import Rest.Battle (restServer)
import Core.Checkers
import Core.CmdLine

  -- let stdout = LoggingSettings $ filtering defaultLogFilter defStdoutSettings
      -- debug = LoggingSettings $ Filtering (\m -> lmLevel m == trace_level) ((defFileSettings "trace.log") {lsFormat = "{time} {source} [{thread}]: {message}\n"})
      -- settings = LoggingSettings $ ParallelLogSettings [stdout, debug]

parserInfo :: ParserInfo (CmdLine ServerCommand)
parserInfo = info (cmdline parseServerCommand <**> helper)
  ( fullDesc
    <> progDesc "HCheckers server application"
    <> header "Run HCheckers server"
    <> footer "Use `+RTS options' after all HCheckers parameters to specify options for GHC Runtime, such as amount of heap to be used; for example, `hcheckersd --local=on +RTS -H1G'. Use `hcheckersd +RTS -?' to display help about Runtime options.")

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    CmdVersion -> V.printVersion
    _ ->
        withCheckers cmd $ do
          when (cmdDumpConfig cmd) $ do
            cfg <- asks csConfig
            liftIO $ print cfg
          logLevel <- asks (gcLogLevel . csConfig)
          let fltr = [([], logLevel)]
          withLogContext (LogContextFrame [] (include fltr)) $ do
              case cmdCommand cmd of
                RunBattleServer -> do
                  bsConfig <- asks (gcBattleServerConfig . csConfig)
                  let host = T.unpack $ bsHost bsConfig
                      port = bsPort bsConfig
                  if bsEnable bsConfig
                     then runRestServer host port Rest.Battle.restServer
                     else fail "Battle server is not enabled in config"
                RunGameServer {} -> do
                  host <- asks (T.unpack . gcHost . csConfig)
                  port <- asks (gcPort . csConfig)
                  runRestServer host port Rest.Game.restServer

