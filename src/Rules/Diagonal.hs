{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Diagonal (DiagonalRussian, diagonal) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Russian
import Rules.Generic

newtype DiagonalRussian = DiagonalRussian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show DiagonalRussian where
  show = rulesName

instance HasTopology DiagonalRussian where
  boardTopology _ = Core.Types.Diagonal

instance HasSideNotation DiagonalRussian where
  sideNotation r = chessSideNotation (boardSize r)

instance GameRules DiagonalRussian where
  type EvaluatorForRules DiagonalRussian = SimpleEvaluator
  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (8, 8)
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
  kingKeyFields _ = mainDiagonal 8

  initPiecesCount _ = 24

  dfltEvaluator r = defaultEvaluator r

  boardNotation _ = boardNotation russian

  parseNotation _ = parseNotation russian

  rulesName _ = "diagonal"

  possibleMoves _ = possibleMoves russian
  mobilityScore _ = mobilityScore russian

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "42"
  getAllAddresses r = addresses8 r

diagonal :: DiagonalRussian
diagonal = DiagonalRussian $
  let rules = russianBase rules
  in  rules

