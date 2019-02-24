{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
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

instance Show Brazilian where
  show = rulesName

instance GameRules Brazilian where
  boardSize _ = (8, 8)

  initBoard rnd _ = initBoard rnd russian

  dfltEvaluator r = SomeEval $ defaultEvaluator r

  boardNotation _ = boardNotation international
  parseNotation _ = parseNotation international

  rulesName _ = "brazilian"

  pdnId _ = "26"

  updateRules r _ = r

  possibleMoves (Brazilian rules) side board = gPossibleMoves rules side board
  mobilityScore (Brazilian rules) side board = gMobilityScore rules side board

  getGameResult = genericGameResult

brazilian :: Brazilian
brazilian = Brazilian $
  let rules = internationalBase rules
  in  rules

