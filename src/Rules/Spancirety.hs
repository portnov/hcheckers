{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Rules.Spancirety (Spancirety (..)) where

import Data.Typeable

import Core.Types
import Core.Board
import Rules.Russian

data Spancirety = Spancirety
  deriving (Show, Eq, Ord, Typeable)

instance GameRules Spancirety where
  initBoard Spancirety =
    let board = buildBoard (8, 10)
        labels1 = ["a1", "c1", "e1", "g1", "i1",
                   "b2", "d2", "f2", "h2", "j2",
                   "a3", "c3", "e3", "g3", "i3"]
        labels2 = ["b8", "d8", "f8", "h8", "j8",
                   "a7", "c7", "e7", "g7", "i7",
                   "b6", "d6", "f6", "h6", "j6"]
    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  boardSize Spancirety = (8, 10)

  boardNotation Spancirety = boardNotation Russian

  parseNotation Spancirety = parseNotation Russian

  rulesName Spancirety = "spancirety"

  possibleMoves Spancirety = possibleMoves Russian

  updateRules Spancirety _ = Spancirety

  getGameResult = genericGameResult

