{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules.Diagonal (Diagonal, diagonal) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Russian
import Rules.Generic

newtype Diagonal = Diagonal GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Evaluator Diagonal where
  evaluatorName _ = "diagonal"
  evalBoard _ = evalBoard defaultEvaluator

instance Show Diagonal where
  show = rulesName

instance GameRules Diagonal where
  initBoard r =
    let board = buildBoard (boardOrientation r) (8, 8)
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

  boardSize _ = (8, 8)

  boardNotation _ = boardNotation russian

  parseNotation _ = parseNotation russian

  rulesName _ = "diagonal"

  possibleMoves _ = possibleMoves russian

  updateRules r _ = r

  getGameResult = genericGameResult

diagonal :: Diagonal
diagonal = Diagonal $
  let rules = russianBase rules
  in  rules

