{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent.STM
import Data.Default
import System.Log.Heavy
import Options.Applicative

import Core.Types
import Core.Board
import AI
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent
import Core.Rest
import Core.Checkers
import Core.CmdLine

import Learn
import Battle
import Rules.Russian

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

    ["battle", path1, path2] -> do
      let rules = russian
      ai1 <- loadAi "default" rules path1
      ai2 <- loadAi "default" rules path2
      withCheckers cmd $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $ do
            runBattle (SomeRules rules) (SomeAi ai1) (SomeAi ai2) "battle.pdn"
            return ()

    ["match", ns, path1, path2] -> do
      let rules = russian
          n = read ns
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

    ["generate", ns, deltas, path] -> do
      let n = read ns
          delta = read deltas
      generateAiVariations n delta path

    -- ["dump", path] -> checkDataFile' path
    ["load", path] -> do
      idx <- loadIndexIO path
      print path

    ["test"] -> do
      withCheckers cmd $ do
        sh <- asks csSupervisor
        st <- liftIO $ atomically $ readTVar sh
        let b = movePiece' "c3" "e5" $ board8 st russian
            b' = flipBoard b
            b'' = flipBoard b'
        liftIO $ do
          print b
          print b'
          print b''
          print (b == b'')
    
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
