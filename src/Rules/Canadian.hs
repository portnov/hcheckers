{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Canadian (Canadian, canadian) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import Rules.International

newtype Canadian = Canadian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Canadian where
  show = rulesName

instance HasTopology Canadian where
  boardTopology _ = Diagonal

instance HasSideNotation Canadian where
  sideNotation r = numericSideNotation (boardSize r)

instance HasBoardSize Canadian where
  boardSize (Canadian r) = boardSize r

instance GameRules Canadian where
  type EvaluatorForRules Canadian = SimpleEvaluator

  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (12, 12)
        labels1 = ["a1", "c1", "e1", "g1", "i1", "k1",
                   "b2", "d2", "f2", "h2", "j2", "l2",
                   "a3", "c3", "e3", "g3", "i3", "k3",
                   "b4", "d4", "f4", "h4", "j4", "l4",
                   "a5", "c5", "e5", "g5", "i5", "k5"]

        labels2 = ["b12", "d12", "f12", "h12", "j12", "l12",
                   "a11", "c11", "e11", "g11", "i11", "k11",
                   "b10", "d10", "f10", "h10", "j10", "l10",
                   "a9", "c9", "e9", "g9", "i9", "k9",
                   "b8", "d8", "f8", "h8", "j8", "l8"]

    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  initPiecesCount _ = 60

  dfltEvaluator r = (defaultEvaluator r) {seKingCoef = 5, seHelpedKingCoef = 6}

  boardNotation r = numericNotation (boardSize r)

  parseNotation r = parseNumericNotation (boardSize r)

  rulesName _ = "canadian"

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "27"

  possibleMoves (Canadian rules) side board = gPossibleMoves rules side board
  mobilityScore (Canadian rules) side board = gMobilityScore rules side board

canadian :: Canadian
canadian = Canadian $
  let rules = (internationalBase rules) {
                gBoardSize = (12, 12)
              }
  in  rules

