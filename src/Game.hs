
module Game where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf
import Data.IORef

import Types
import Board
-- import Russian

import Debug.Trace

data Game = Game {
    gThread :: ThreadId
  , gInput :: Chan GameRq
  , gOutput :: Chan GameRs
  }

data GameRq =
    Exit
  | DoMoveRq Side Move
  | PossibleMovesRq Side
  | BoardRq

data GameRs =
    Ok
  | DoMoveRs Board [PushMsg]
  | PossibleMovesRs [Move]
  | BoardRs Board
  | Error String

data PushMsg = PushMsg Side Move Board

spawnGame :: GameRules rules => rules -> IO Game
spawnGame rules = do
    input <- newChan
    output <- newChan
    thread <- forkIO (setup input output)
    return $ Game thread input output

  where
    setup input output = do
      let board = initBoard rules
      let side = First
      loop input output board side

    loop input output board side = do
      rq <- readChan input
      case rq of
        Exit -> writeChan output Ok

        PossibleMovesRq s -> do
          let moves = possibleMoves rules s board
          writeChan output (PossibleMovesRs moves)
          loop input output board side

        BoardRq -> do
          writeChan output (BoardRs board)
          loop input output board side

        DoMoveRq s move -> do
          if s /= side
            then do
                 writeChan output (Error "Not your turn")
                 loop input output board side
            else do
                 if move `notElem` possibleMoves rules side board
                   then do
                        writeChan output (Error "Not allowed move")
                        loop input output board side
                   else do
                        let (board', _, _) = applyMove side move board
                            push = PushMsg (opposite side) move board'
                        writeChan output $ DoMoveRs board' [push]
                        loop input output board' (opposite side)

      

