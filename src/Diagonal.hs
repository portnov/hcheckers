{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Diagonal (Diagonal (..)) where

import qualified Data.Text as T
import Data.Typeable
import Data.String
import Data.Char
import Data.List

import Types
import Board
import Russian

data Diagonal = Diagonal
  deriving (Show, Eq, Ord, Typeable)

instance GameRules Diagonal where
  initBoard Diagonal =
    let board = buildBoard (8, 8)
        labels1 = ["c1", "e1", "g1",
                   "d2", "f2", "h2",
                   "e3", "g3",
                   "f4", "h4",
                   "g5",
                   "h6"]
        labels2 = ["b8", "d8", "f8",
                   "a7", "c7", "e7",
                   "b6", "d6",
                   "a5", "c5",
                   "b4",
                   "a3"]
    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  boardSize Diagonal = (8, 8)

  boardNotation Diagonal = boardNotation Russian

  parseNotation Diagonal = parseNotation Russian

  rulesName Diagonal = "diagonal"

  possibleMoves Diagonal = possibleMoves Russian

  updateRules Diagonal _ = Diagonal

  getGameResult = genericGameResult

