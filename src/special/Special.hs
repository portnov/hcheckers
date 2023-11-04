{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import Control.Monad
import Control.Monad.Reader
import Data.Maybe (isJust)
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Format.Heavy
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Aeson as Aeson
import System.Log.Heavy
import System.Log.Heavy.TH
import qualified Options.Applicative as O
import Options.Applicative ((<**>))
import Text.Printf

import Core.Types hiding (timed)
import qualified Core.Version as V
import Core.Board (parseBoardRep)
import Core.Checkers
import Core.CmdLine
import Core.Supervisor (withRules)
import Core.Monitoring
import Core.Evaluator (preEval, defaultEvaluator)
import Formats.Fen (parseFen)
import Formats.Pdn (parseSemiMove')
import Rules.Russian
import AI
import AI.AlphaBeta.Persistent (loadAiData')
import AI.AlphaBeta.Types

import Learn
import Battle

moduleToStdout mod cfg logChan =
  let dfltBackends = dfltLoggingSettings cfg logChan
      stdout = defStdoutSettings {
                lsFormat = "{time} [G#{generation}; St.{stage}; M.{match}; B.{battle}] {message}\n"
              }
      filteredStdout = LoggingSettings $ Filtering (\m -> lmSource m == [mod]) stdout
  in  ParallelLogSettings $ dfltBackends ++ [filteredStdout]

parserInfo :: O.ParserInfo (CmdLine SpecialCommand)
parserInfo = O.info (cmdline parseSpecialCommand <**> O.helper)
  ( O.fullDesc
    <> O.progDesc "HCheckers special utility application"
    <> O.header "Execute specialized commands"
    <> O.footer "Use `+RTS options' after all HCheckers parameters to specify options for GHC Runtime, such as amount of heap to be used; for example, `hcheckersd --local=on +RTS -H1G'. Use `hcheckersd +RTS -?' to display help about Runtime options.")

main :: IO ()
main = do
  cmd <- O.execParser parserInfo
  case cmd of
    CmdVersion -> V.printVersion
    _ -> special cmd

dumpConfig :: CmdLine SpecialCommand -> Checkers ()
dumpConfig cmd =
    when (cmdDumpConfig cmd) $ do
        cfg <- asks csConfig
        liftIO $ print cfg

parseSide :: Maybe Int -> Side
parseSide (Just 1) = First
parseSide (Just 2) = Second
parseSide (Just _) = error "impossible"
parseSide Nothing = First

special :: CmdLine SpecialCommand -> IO ()
special cmd =
  case cmdCommand cmd of
    Learn rulesName aiPath pdnPath -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules aiPath
        withCheckersLog cmd (moduleToStdout "Learn") $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $
              learnPdnOrDir ai pdnPath
            printCurrentMetrics (Just "ai.")
            printCurrentMetrics (Just "learn.")

    EvalBoard rulesName mbAiPath mbSideNr fenPath -> do
      fenText <- TIO.readFile fenPath
      withRules rulesName $ \rules -> do
        let side = parseSide mbSideNr
            isForSide = isJust mbSideNr
        case parseFen (SomeRules rules) fenText of
          Left err -> fail err
          Right (_,brep) -> do
            let board = parseBoardRep DummyRandomTableProvider rules brep
            case mbAiPath of
              Nothing -> do
                let eval = defaultEvaluator rules
                if isForSide
                  then do
                    liftIO $ print $ preEval eval side board
                  else do
                    liftIO $ print $ evalBoard eval First board
              Just aiPath -> do
                (AlphaBeta _ _ eval) <- loadAi "default" rules aiPath
                if isForSide
                  then do
                    fail "Can't use arbitrary AI for one-sided board evaluation"
                  else do
                    liftIO $ print $ evalBoard eval First board

    EvalMove rulesName aiPath mbSideNr fenPath moveStr -> do
      fenText <- TIO.readFile fenPath
      withRules rulesName $ \rules -> do
        let side = parseSide mbSideNr
        case parseFen (SomeRules rules) fenText of
          Left err -> fail err
          Right (_,brep) -> do
            let board = parseBoardRep DummyRandomTableProvider rules brep
            case parseSemiMove' (SomeRules rules) side board (T.pack moveStr) of
              Left err -> fail $ show err
              Right pm -> do
                ai <- loadAi "default" rules aiPath
                withCheckersLog cmd (moduleToStdout "Learn") $ do
                  dumpConfig cmd
                  withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
                    result <- scoreMove' ai side board pm
                    liftIO $ print result

    Openings rulesName aiPath depth -> do
      withRules rulesName $ \rules -> do
        ai <- loadAi "default" rules aiPath
        withCheckersLog cmd (moduleToStdout "Learn") $ do
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

    Battle rulesName Nothing path1 path2 -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runBattleLocal (SomeRules rules) (1,SomeAi ai1) (2,SomeAi ai2) "battle.pdn"
              return ()
            printCurrentMetrics Nothing

    Battle rulesName (Just host) path1 path2 -> do
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
        withCheckersLog cmd (moduleToStdout "Battle") $ do
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
            runTournamentDumb (dumbMatchRunner runBattleLocal) rules ais nMatches nGames
            return ()

    Olympics rulesName nGames nBest paths -> do
      withRules rulesName $ \rules -> do
        ais <- forM paths $ \path -> loadAi "default" rules path
        withCheckersLog cmd (moduleToStdout "Battle") $ do
            dumpConfig cmd
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              let cais = prepareCandidates ais
              results <- runTournamentOlympic (dumbMatchRunner runBattleLocal) rules cais nGames nBest
              $info "Winners are: {}" (Single $ show $ map cName results)
              return ()

    Genetics yamlPath -> do
      withCheckersLog cmd (moduleToStdout "Battle") $ do
          dumpConfig cmd
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            result <- runGeneticsJ yamlPath
            forM_ result $ \(SomeAi ai) -> liftIO $ C8.putStrLn $ Aeson.encode ai
            return ()

    GenerateBased path n delta -> do
      generateAiVariationsBased n delta path

    GenerateFromZero path maxValue -> do
      generateAiVariationsFromZero (fromIntegral maxValue) path

    Dump path -> do
      withCheckers cmd $ do
        dumpConfig cmd
        let rules = russian
        (vec, bmap) <- loadAiData' path
        liftIO $ printf "Evaluator: %s\n" (show vec)
        forM_ (M.assocs bmap) $ \(bHash, item) -> do
            liftIO $ printf "Hash: %d => %s\n" bHash (show item)

