{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Brazilian (Brazilian, brazilian) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import Rules.International
import Rules.Russian

newtype Brazilian = Brazilian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance HasTopology Brazilian where
  boardTopology _ = Diagonal

instance Show Brazilian where
  show = rulesName

instance HasSideNotation Brazilian where
  sideNotation r = numericSideNotation (boardSize r)

instance HasBoardSize Brazilian where
  boardSize _ = (8, 8)

instance GameRules Brazilian where
  type EvaluatorForRules Brazilian = SimpleEvaluator

  kingKeyFields _ = mainDiagonal 8

  initBoard rnd _ = initBoard rnd russian

  initPiecesCount _ = 24

  dfltEvaluator r = defaultEvaluator r

  boardNotation _ = boardNotation international
  parseNotation _ = parseNotation international

  rulesName _ = "brazilian"

  pdnId _ = "26"

  updateRules r _ = r

  possibleMoves (Brazilian rules) side board = gPossibleMoves rules side board
  hasCapturesOrPromotions (Brazilian rules) side board = genericHasCapturesOrPromotions rules side board
  mobilityScore (Brazilian rules) side board = gMobilityScore rules side board
  isManBlockedByKing = genericIsManBlockedByKing

  getGameResult = genericGameResult
  getAllAddresses r = addresses8 r

brazilian :: Brazilian
brazilian = Brazilian $
  let rules = internationalBase rules
  in  rules

