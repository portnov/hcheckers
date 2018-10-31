{-# LANGUAGE OverloadedStrings #-}

import qualified Control.Monad.Metrics as Metrics
import Data.Default
import System.Environment
import System.Log.Heavy
import qualified System.Metrics as EKG
import qualified System.Remote.Monitoring as EKG
import Lens.Micro ((^.))

import Core.Types
import AI.AlphaBeta.Types
import AI.AlphaBeta.Persistent
import Core.Rest
import Core.Supervisor
import Core.Board

import Learn
import Rules.Russian

main :: IO ()
main = do
    let b = buildBoard (FirstAtBottom) (8, 8)
        labels1 = ["a5"]
        labels2 = ["b6", "f6", "h6"]
        board = setManyPieces' labels1 (Piece King First) $ setManyPieces' labels2 (Piece Man Second) b
        moves = possibleMoves Russian First board
    print moves

