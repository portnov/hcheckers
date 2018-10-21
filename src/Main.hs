module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Text.Printf
import Data.Default
import System.Environment
import System.Log.Heavy

import Types
import Board
import Russian
import AI
import AICache
import Rest
import Learn
import Supervisor

withCheckers :: LoggingSettings -> SupervisorHandle -> Checkers a -> IO a
withCheckers (LoggingSettings settings) supervisor actions =
  withLoggingB settings $ \backend ->
    let logger = makeLogger backend
        logging = LoggingTState logger (AnyLogBackend backend) []
        cs = CheckersState logging supervisor
    in  runCheckersT actions cs

main :: IO ()
main = do
  args <- getArgs
  let settings = LoggingSettings defStdoutSettings
  supervisor <- mkSupervisor
  case args of
    ["learn", path] -> do
      let rules = Russian
          params = def :: AlphaBetaParams
          ai = AlphaBeta params rules
          eval = ai
          depth = 6
      withCheckers settings supervisor $
          learnPdn ai path depth
    
    _ ->
      withCheckers settings supervisor $
          withLogContext (LogContextFrame [] (include defaultLogFilter)) $
              runRestServer


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
