{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Armenian (
    Armenian, armenian, armenianBase
  ) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic

newtype Armenian = Armenian GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Armenian where
  show = rulesName

instance HasTopology Armenian where
  boardTopology _ = DiagonalAndOrthogonal

instance SimpleEvaluatorSupport Armenian where
  getBackDirections _ = [Backward]
  getForwardDirections _ = [ForwardLeft, Forward, ForwardRight]

instance GameRules Armenian where
  type EvaluatorForRules Armenian = SimpleEvaluator

  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (8, 8)
        labels1 = ["a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
                   "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3"]
        labels2 = ["a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
                   "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6"]
    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  initPiecesCount _ = 32

  boardSize _ = (8, 8)

  dfltEvaluator r = (defaultEvaluator r) {
                                 seKingCoef = 4,
                                 seHelpedKingCoef = 5,
                                 seOppositeSideWeight = 5,
                                 seMobilityWeight = 4,
                                 seBackedWeight = 1,
                                 seBorderMenBad = False
                                }

  boardNotation _ = chessNotation

  parseNotation _ = parseChessNotation

  rulesName _ = "armenian"

  possibleMoves (Armenian rules) side board = gPossibleMoves rules side board

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "44"

armenianBase :: GenericRules -> GenericRules
armenianBase =
  let rules this = (abstractRules this) {
                        gManSimpleMoveDirections = [PLeft, ForwardLeft, Forward, ForwardRight, PRight]
                      , gManCaptureDirections =  [PLeft, Forward, PRight]
                      , gKingCaptureDirections = [Backward, PLeft, Forward, PRight]
                      , gKingSimpleMoveDirections = [Backward, BackwardLeft, PLeft, ForwardLeft,
                                                     Forward, ForwardRight, PRight, BackwardRight]
                      , gManCaptures = manCaptures this
                      , gCaptureMax = True
                      , gRemoveCapturedImmediately = False
                    }
  in  rules

armenian :: Armenian
armenian = Armenian $
  let rules = armenianBase rules
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

