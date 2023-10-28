{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import Control.Monad
import Control.Monad.Reader
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Aeson as Aeson
import System.Log.Heavy
import Options.Applicative
import Text.Printf

import Core.Types hiding (timed)
import qualified Core.Version as V
import AI
import AI.AlphaBeta.Persistent (loadAiData')
import Core.Checkers
import Core.CmdLine
import Core.Supervisor (withRules)
import Core.Monitoring

import Learn
import Battle
import Rules.Russian

learnLogging cfg logChan =
  let dfltBackends = dfltLoggingSettings cfg logChan
      stdout = LoggingSettings $ Filtering (\m -> lmSource m == ["Learn"]) defStdoutSettings
  in  ParallelLogSettings $ dfltBackends ++ [stdout]

parserInfo :: ParserInfo (CmdLine SpecialCommand)
parserInfo = info (cmdline parseSpecialCommand <**> helper)
  ( fullDesc
    <> progDesc "HCheckers special utility application"
    <> header "Execute specialized commands"
    <> footer "Use `+RTS options' after all HCheckers parameters to specify options for GHC Runtime, such as amount of heap to be used; for example, `hcheckersd --local=on +RTS -H1G'. Use `hcheckersd +RTS -?' to display help about Runtime options.")

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmd of
    CmdVersion -> V.printVersion
    _ -> special cmd

dumpConfig :: CmdLine SpecialCommand -> Checkers ()
dumpConfig cmd =
    when (cmdDumpConfig cmd) $ do
        cfg <- asks csConfig
        liftIO $ print cfg

special :: CmdLine SpecialCommand -> IO ()
special cmd =
  case cmdCommand cmd of
    Learn rulesName aiPath pdnPath -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules aiPath
        withCheckersLog cmd learnLogging $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $
              learnPdnOrDir ai pdnPath
            printCurrentMetrics (Just "ai.")
            printCurrentMetrics (Just "learn.")

    Openings rulesName aiPath depth -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules aiPath
        withCheckersLog cmd learnLogging $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $
              analyzeOpenings ai depth

    Bench rulesName path n -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules path
        withCheckers cmd $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              replicateM_ n $
                  runBattleLocal (SomeRules rules) (1,SomeAi ai) (2,SomeAi ai) "battle.pdn"
              return ()
            printCurrentMetrics (Just "ai.")

    Battle rulesName path1 path2 -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runBattleLocal (SomeRules rules) (1,SomeAi ai1) (2,SomeAi ai2) "battle.pdn"
              return ()
            printCurrentMetrics Nothing

    RemoteBattle host rulesName path1 path2 -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              rs <- runBattleRemote (T.pack host) (SomeRules rules) (1,SomeAi ai1) (2,SomeAi ai2) "battle.pdn"
              liftIO $ putStrLn $ "Remote: " ++ show rs
              return ()

    Match rulesName n path1 path2 -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        putStrLn $ "AI1: " ++ show ai1
        putStrLn $ "AI2: " ++ show ai2
        withCheckers cmd $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runMatch runBattleLocal (SomeRules rules) (1, SomeAi ai1) (2, SomeAi ai2) n
              return ()
            printCurrentMetrics Nothing

    Tournament nMatches nGames paths -> do 
      let rules = russian
      ais <- forM paths $ \path -> loadAi "default" rules path
      withCheckers cmd $ do
          dumpConfig cmd
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            runTournament (dumbMatchRunner runBattleLocal) rules ais nMatches nGames
            return ()

    Genetics yamlPath -> do
      withCheckers cmd $ do
          dumpConfig cmd
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            result <- runGeneticsJ yamlPath
            forM_ result $ \(SomeAi ai) -> liftIO $ C8.putStrLn $ Aeson.encode ai
            return ()

    Generate n delta path -> do
      generateAiVariations n (fromIntegral delta) path

    Dump path -> do
      withCheckers cmd $ do
        dumpConfig cmd
        let rules = russian
        (vec, bmap) <- loadAiData' path
        liftIO $ printf "Evaluator: %s\n" (show vec)
        forM_ (M.assocs bmap) $ \(bHash, item) -> do
            liftIO $ printf "Hash: %d => %s\n" bHash (show item)

