{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Default
import qualified Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Aeson as Aeson
import System.Log.Heavy
import Options.Applicative
import Text.Printf

import Core.Types
import Core.Board
import AI
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent
import Core.Rest
import Core.Checkers
import Core.CmdLine
import Core.Supervisor (withRules)
import Core.Evaluator

import Learn
import Battle
import Rules.Russian
import Rules.Spancirety

  -- let stdout = LoggingSettings $ filtering defaultLogFilter defStdoutSettings
      -- debug = LoggingSettings $ Filtering (\m -> lmLevel m == trace_level) ((defFileSettings "trace.log") {lsFormat = "{time} {source} [{thread}]: {message}\n"})
      -- settings = LoggingSettings $ ParallelLogSettings [stdout, debug]

main :: IO ()
main = do
  cmd <- execParser parserInfo
  case cmdSpecial cmd of
    Nothing ->
      withCheckers cmd $ do
          cfg <- asks csConfig
          let fltr = [([], gcLogLevel cfg)]
          withLogContext (LogContextFrame [] (include fltr)) $
              runRestServer
    Just str -> special cmd (words str)

special :: CmdLine -> [String] -> IO ()
special cmd args =
  case args of
    ["learn", path] -> do
      let rules = russian
          eval = ai
          params = def {
                     abDepth = 4
                   , abCombinationDepth = 9
                   }
          ai = AlphaBeta params rules (dfltEvaluator rules)
      withCheckers cmd $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $
            learnPdn ai path

    ["battle", rulesName, path1, path2] -> do
      withRules rulesName $ \rules -> do
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        withCheckers cmd $
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runBattle (SomeRules rules) (SomeAi ai1) (SomeAi ai2) "battle.pdn"
              return ()

    ["match", rulesName, ns, path1, path2] -> do
      withRules rulesName $ \rules -> do
        let n = read ns
        ai1 <- loadAi "default" rules path1
        ai2 <- loadAi "default" rules path2
        putStrLn $ "AI1: " ++ show ai1
        putStrLn $ "AI2: " ++ show ai2
        withCheckers cmd $
            withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
              runMatch (SomeRules rules) (SomeAi ai1) (SomeAi ai2) n
              return ()

    ("tournament": matches : games : paths) -> do 
      let rules = russian
          nMatches = read matches
          nGames = read games
      ais <- forM paths $ \path -> loadAi "default" rules path
      withCheckers cmd $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            runTournament rules ais nMatches nGames
            return ()

    ("genetics": generations : size : best : paths) -> do
      let rules = spancirety
          nGenerations = read generations
          generationSize = read size
          nBest = read best
      ais <- forM paths $ \path -> loadAi "default" rules path
      withCheckers cmd $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            result <- runGenetics rules nGenerations generationSize nBest ais
            forM_ result $ \ai -> liftIO $ C8.putStrLn $ Aeson.encode ai
            return ()

    ["generate", ns, deltas, path] -> do
      let n = read ns
          delta = read deltas
      generateAiVariations n delta path

    ["test"] -> do
      withCheckers cmd $ do
        let rules = russian
            cache = seCache $ defaultEvaluator rules
        forM_ (M.assocs cache) $ \(addr, sed) -> do
            liftIO $ printf "%s => %s, %s\n"
                (show $ aLabel addr)
                (show $ weightForSide First $ sedCenter sed)
                (show $ weightForSide Second $ sedCenter sed)
    
-- main :: IO ()
-- main = do
--   let a3 = resolve "a3" board8
--   let b4 = resolve "b4" board8
--   let c5 = resolve "c5" board8
--   let b6 = resolve "b6" board8
--   let board8' = movePiece' "h8" "b4" $ removePiece' "a7" board8
--   let capture = simpleCapture First a3 ForwardRight
--   putStrLn "1."
--   print capture
-- 
--   putStrLn "2."
--   let (board', addr', p) = applyMove First capture board8'
--   print $ getPiece a3 board'
--   print $ getPiece b4 board'
--   print $ getPiece c5 board'
--   print $ getPiece b6 board'
-- 
--   let capture' = simpleCapture First c5
-- 
--   putStrLn "3."
--   print $
--     let addr = resolve "c5" board'
--         piece = fromJust $ getPiece addr board'
--     in  captures1 piece board' addr
-- 
--   putStrLn "4."
--   print $
--     let addr = resolve "a3" board8'
--         piece = fromJust $ getPiece addr board8'
--     in  manCaptures piece board8' addr
-- 
--   putStrLn "5."
--   print $ possibleMoves Russian First board8'
-- 
--   putStrLn "6."
--   let board8'' = setPiece' "b2" (Piece King First) $
--                  setManyPieces' ["d4", "d6", "g7"] (Piece Man Second) $ buildBoard 8
--   print $ kingSimpleMoves First board8'' (resolve "b2" board8'')
-- 
--   putStrLn "7."
--   print $ possibleMoves Russian First board8''
-- 
--   putStrLn "8."
--   let board = board8''
--   let moves = possibleMoves Russian Second board
--   forM_ moves $ \move -> do
--     print move
--     let (board', addr', _) = applyMove Second move board
--         moves' = possibleMoves Russian Second board'
--     let score1 = evalBoard Russian First board'
--         score2 = evalBoard Russian Second board'
--     putStrLn $ show score1 ++ " vs " ++ show score2
-- 
--   putStrLn "9."
--   let board = movePiece' "e3" "f4" board8
--   let moves = possibleMoves Russian Second board
--   forM_ moves $ \move -> do
--     let (board', addr', _) = applyMove Second move board
--         moves' = possibleMoves Russian Second board'
--     let score1 = evalBoard Russian First board'
--         score2 = evalBoard Russian Second board'
--     putStrLn $ show move ++ " => " ++ show score1 ++ " vs " ++ show score2
-- 
--   putStrLn "10."
--   let board = setManyPieces' ["c3", "e3"] (Piece Man First) $ 
--               setManyPieces' ["e5", "f6"] (Piece Man Second) $ buildBoard 8
--   let ai = AlphaBeta 2 Russian Russian
--   print =<< chooseMove ai Second board
-- 
-- 
--   putStrLn "11."
--   let board = setManyPieces' ["c3", "e3"] (Piece Man First) $ 
--               setManyPieces' ["e5", "f6"] (Piece Man Second) $ buildBoard 8
--   let ai = AlphaBeta 2 Russian Russian
--   print =<< chooseMove ai First board
-- 
-- 
