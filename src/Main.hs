{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import Data.Default
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Aeson as Aeson
import System.Log.Heavy
import Options.Applicative
import Text.Printf

import Core.Types hiding (timed)
import AI
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent (loadAiData')
import Rest.Common (runRestServer)
import Rest.Game (restServer)
import Rest.Battle (restServer)
import Core.Checkers
import Core.CmdLine
import Core.Supervisor (withRules)
import Core.Monitoring

import Learn
import Battle
import Rules.Russian
-- import Rules.Spancirety

  -- let stdout = LoggingSettings $ filtering defaultLogFilter defStdoutSettings
      -- debug = LoggingSettings $ Filtering (\m -> lmLevel m == trace_level) ((defFileSettings "trace.log") {lsFormat = "{time} {source} [{thread}]: {message}\n"})
      -- settings = LoggingSettings $ ParallelLogSettings [stdout, debug]

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmdSpecial cmd of
    Nothing ->
      withCheckers cmd $ do
          logLevel <- asks (gcLogLevel . csConfig)
          host <- asks (T.unpack . gcHost . csConfig)
          port <- asks (gcPort . csConfig)
          bsConfig <- asks (gcBattleServerConfig . csConfig)
          let fltr = [([], logLevel)]
          withLogContext (LogContextFrame [] (include fltr)) $ do
              if fromMaybe False (cmdBattleServer cmd)
                then if bsEnable bsConfig
                       then runRestServer (T.unpack $ bsHost bsConfig) (bsPort bsConfig) Rest.Battle.restServer
                       else fail "Battle server is not enabled in config"
                else runRestServer host port Rest.Game.restServer
                
    Just str -> special cmd (words str)

special :: CmdLine -> [String] -> IO ()
special cmd args =
  case args of
    ["learn", rulesName, aiPath, pdnPath] -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules aiPath
        withCheckers cmd $ do
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $
              learnPdnOrDir ai pdnPath
            printCurrentMetrics (Just "ai.")
            printCurrentMetrics (Just "learn.")

    ["bench", rulesName, path, ns] -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules path
        withCheckers cmd $ do
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              replicateM_ (read ns) $
                  runBattleLocal (SomeRules rules) (1,SomeAi ai) (2,SomeAi ai) "battle.pdn"
              return ()
            printCurrentMetrics (Just "ai.")

    ["battle", rulesName, path1, path2] -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $ do
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runBattleLocal (SomeRules rules) (1,SomeAi ai1) (2,SomeAi ai2) "battle.pdn"
              return ()
            printCurrentMetrics Nothing

    ["battle-remote", host, rulesName, path1, path2] -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              rs <- runBattleRemote (T.pack host) (SomeRules rules) (1,SomeAi ai1) (2,SomeAi ai2) "battle.pdn"
              liftIO $ putStrLn $ "Remote: " ++ show rs
              return ()

    ["match", rulesName, ns, path1, path2] -> do
      withRules rulesName $ \rules -> do
        let n = read ns
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        putStrLn $ "AI1: " ++ show ai1
        putStrLn $ "AI2: " ++ show ai2
        withCheckers cmd $ do
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runMatch runBattleLocal (SomeRules rules) (1, SomeAi ai1) (2, SomeAi ai2) n
              return ()
            printCurrentMetrics Nothing

    ("tournament": matches : games : paths) -> do 
      let rules = russian
          nMatches = read matches
          nGames = read games
      ais <- forM paths $ \path -> loadAi "default" rules path
      withCheckers cmd $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            runTournament (dumbMatchRunner runBattleLocal) rules ais nMatches nGames
            return ()

    ["genetics", yamlPath] -> do
      withCheckers cmd $ do
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            result <- runGeneticsJ yamlPath
            forM_ result $ \(SomeAi ai) -> liftIO $ C8.putStrLn $ Aeson.encode ai
            return ()

    ["generate", ns, deltas, path] -> do
      let n = read ns
          delta = read deltas
      generateAiVariations n delta path

    ["dump", path] -> do
      withCheckers cmd $ do
        let rules = russian
        (vec, bmap) <- loadAiData' path
        liftIO $ printf "Evaluator: %s\n" (show vec)
        forM_ (M.assocs bmap) $ \(bHash, item) -> do
            liftIO $ printf "Hash: %d => %s\n" bHash (show item)

