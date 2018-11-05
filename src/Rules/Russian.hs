{-# LANGUAGE DeriveDataTypeable #-}
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

manCaptures :: GenericRules -> Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove]
manCaptures rules mbPrevDir captured piece@(Piece _ side) board src =
  let captures = gPieceCaptures1 rules mbPrevDir captured piece board src
      nextMoves pm = gPieceCaptures rules (Just $ firstMoveDirection m) captured' p' b (pmEnd pm)
                        where p' = if pmPromote pm then promotePiece piece else piece
                              b = setPiece (pmEnd pm) p' board
                              m = pmMove pm
                              captured' = foldr insertLabelSet captured (map aLabel $ pmVictims pm)
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture piece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

