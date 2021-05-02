{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Spancirety (Spancirety, spancirety) where

import Data.Typeable
import qualified Data.IntMap as IM

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import Rules.Russian

newtype Spancirety = Spancirety GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Spancirety where
  show = rulesName

instance HasTopology Spancirety where
  boardTopology _ = Diagonal

instance GameRules Spancirety where
  type EvaluatorForRules Spancirety = SimpleEvaluator
  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (8, 10)
        labels1 = ["a1", "c1", "e1", "g1", "i1",
                   "b2", "d2", "f2", "h2", "j2",
                   "a3", "c3", "e3", "g3", "i3"]
        labels2 = ["b8", "d8", "f8", "h8", "j8",
                   "a7", "c7", "e7", "g7", "i7",
                   "b6", "d6", "f6", "h6", "j6"]
    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  initPiecesCount _ = 30

  boardSize _ = (8, 10)

  boardNotation _ = boardNotation russian

  dfltEvaluator r = defaultEvaluator r

  parseNotation _ = parseNotation russian

  rulesName _ = "spancirety"

  possibleMoves _ = possibleMoves russian
  mobilityScore _ = mobilityScore russian

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "41"
  getAllAddresses r = IM.elems $ bAddresses $ buildBoard DummyRandomTableProvider r FirstAtBottom (8, 10)

spancirety :: Spancirety
spancirety = Spancirety $
  let rules = russianBase rules
  in  rules

