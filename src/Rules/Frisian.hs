{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Frisian (
    Frisian, frisian, frisianBase
  ) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import qualified Rules.International as I

newtype Frisian = Frisian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Frisian where
  show = rulesName

instance HasTopology Frisian where
  boardTopology _ = FrisianTopology

instance HasSideNotation Frisian where
  sideNotation r = numericSideNotation (boardSize r)

instance GameRules Frisian where
  type EvaluatorForRules Frisian = SimpleEvaluator

  boardSize _ = (10, 10)

  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (10, 10)
        labels1 = ["a1", "c1", "e1", "g1", "i1",
                   "b2", "d2", "f2", "h2", "j2",
                   "a3", "c3", "e3", "g3", "i3",
                   "b4", "d4", "f4", "h4", "j4"]

        labels2 = ["b10", "d10", "f10", "h10", "j10",
                   "a9", "c9", "e9", "g9", "i9",
                   "b8", "d8", "f8", "h8", "j8",
                   "a7", "c7", "e7", "g7", "i7"]

    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  pdnId _ = "40"

  initPiecesCount _ = 40

  boardNotation r = numericNotation (boardSize r)

  dfltEvaluator r = (defaultEvaluator r) {seKingCoef = 5, seHelpedKingCoef = 6}

  parseNotation r = parseNumericNotation (boardSize r)

  rulesName _ = "frisian"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (Frisian rules) side board = gPossibleMoves rules side board
  mobilityScore (Frisian rules) side board = gMobilityScore rules side board

  getAllAddresses r = addresses10 r

  getBackDirections _ = [PLeft, ForwardLeft, Backward, ForwardRight, PRight]
  getForwardDirections _ = [ForwardLeft, Forward, ForwardRight]
  getManSimpleMoveDirections (Frisian r) = gManSimpleMoveDirections r

frisianBase :: GenericRules -> GenericRules
frisianBase =
  let rules this = (abstractRules this) {
                        gManSimpleMoveDirections = [ForwardLeft, ForwardRight]
                      , gManCaptureDirections =  [ForwardLeft, ForwardRight,
                                                  BackwardLeft, BackwardRight,
                                                  Forward, PRight, Backward, PLeft]
                      , gKingCaptureDirections = [ForwardLeft, ForwardRight,
                                                  BackwardLeft, BackwardRight,
                                                  Forward, PRight, Backward, PLeft]
                      , gKingSimpleMoveDirections = [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
                      , gManCaptures = I.manCaptures this
                      , gCaptureMax = True
                      , gRemoveCapturedImmediately = False
                    }
  in  rules

frisian :: Frisian
frisian = Frisian $
  let rules = frisianBase rules
  in  rules

