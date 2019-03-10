{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules.Russian (
        Russian, russian, russianBase
      ) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic

-- import Debug.Trace

newtype Russian = Russian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Russian where
  show = rulesName

instance GameRules Russian where
  initBoard rnd _ = board8 rnd

  boardSize _ = (8, 8)

  boardNotation _ = chessNotation

  dfltEvaluator r = SomeEval $ defaultEvaluator r

  parseNotation _ = parseChessNotation

  rulesName _ = "russian"

  possibleMoves (Russian rules) side board = gPossibleMoves rules side board
  mobilityScore (Russian rules) side board = gMobilityScore rules side board

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "25"

russianBase :: GenericRules -> GenericRules
russianBase =
  let rules this = abstractRules this {
                gManCaptures = manCaptures this
              }
  in  rules

russian :: Russian
russian = Russian $
  let rules = russianBase rules
  in  rules

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let captures = gPieceCaptures1 rules ct
      -- when last horizontal reached, pass promoted piece to 
      -- next moves check; so it will continue capture as a king
      -- if it can
      nextMoves pm = genericNextMoves rules ct True pm
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

