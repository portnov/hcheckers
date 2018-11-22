{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules.Russian (
        Russian, russian, russianBase
      ) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Evaluator
import Rules.Generic

-- import Debug.Trace

newtype Russian = Russian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Russian where
  show = rulesName

instance Evaluator Russian where
  evaluatorName _ = "russian"
  evalBoard rules = evalBoard defaultEvaluator

instance GameRules Russian where
  initBoard _ = board8

  boardSize _ = (8, 8)

  boardNotation _ = chessNotation

  parseNotation _ = parseChessNotation

  rulesName _ = "russian"

  possibleMoves (Russian rules) side board = gPossibleMoves rules side board

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
      nextMoves pm = gPieceCaptures rules $ ct {
                                              ctPrevDirection = Just (firstMoveDirection m),
                                              ctCaptured = captured',
                                              ctPiece = p',
                                              ctBoard = b,
                                              ctCurrent = pmEnd pm
                                            }
                        where p' = if pmPromote pm then promotePiece ctPiece else ctPiece
                              b = setPiece (pmEnd pm) p' ctBoard
                              m = pmMove pm
                              captured' = foldr insertLabelSet ctCaptured (map aLabel $ pmVictims pm)
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

