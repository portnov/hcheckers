{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
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

instance HasTopology Russian where
  boardTopology _ = Diagonal

instance HasSideNotation Russian where
  sideNotation r = chessSideNotation (boardSize r)

instance HasBoardSize Russian where
  boardSize _ = (8, 8)

instance GameRules Russian where
  type EvaluatorForRules Russian = SimpleEvaluator
  initBoard rnd r = board8 rnd r

  kingKeyFields _ = mainDiagonal 8

  initPiecesCount _ = 24

  boardNotation _ = chessNotation

  dfltEvaluator r = defaultEvaluator r

  parseNotation _ = parseChessNotation

  rulesName _ = "russian"

  possibleMoves (Russian rules) side board = gPossibleMoves rules side board
  hasCapturesOrPromotions (Russian rules) side board = genericHasCapturesOrPromotions rules side board
  mobilityScore (Russian rules) side board = gMobilityScore rules side board
  isManBlockedByKing = genericIsManBlockedByKing

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "25"
  getAllAddresses r = addresses8 r

russianBase :: GenericRules -> GenericRules
russianBase =
  let rules this = (abstractRules this) {
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

