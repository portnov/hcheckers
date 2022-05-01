{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Turkish (
    Turkish, turkish, turkishBase
  ) where

import Data.Typeable

import Core.Types
import Core.LabelSet as LS
import Core.Board
import Core.BoardMap
import Core.Evaluator
import Rules.Generic

newtype Turkish = Turkish GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Turkish where
  show = rulesName

instance HasTopology Turkish where
  boardTopology _ = Orthogonal

instance HasSideNotation Turkish where
  sideNotation r = chessSideNotation (boardSize r)

instance GameRules Turkish where
  type EvaluatorForRules Turkish = SimpleEvaluator
  initBoard rnd r =
    let board = buildBoard rnd r (boardOrientation r) (8, 8)
        labels1 = ["a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
                   "a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3"]
        labels2 = ["a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
                   "a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6"]
    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  initPiecesCount _ = 32

  boardSize _ = (8, 8)

  dfltEvaluator r = (defaultEvaluator r) {seKingCoef = 5, seHelpedKingCoef = 6, seBorderManWeight = 0}

  boardNotation _ = chessNotation

  parseNotation _ = parseChessNotation

  rulesName _ = "turkish"

  possibleMoves (Turkish rules) side board = gPossibleMoves rules side board

  updateRules r _ = r

  getGameResult = genericGameResult

  pdnId _ = "43"

  getBackDirections _ = [Backward]
  getAllAddresses r = addresses8 r

turkishBase :: GenericRules -> GenericRules
turkishBase =
  let rules this = (abstractRules this) {
                        gManSimpleMoveDirections = [PLeft, Forward, PRight]
                      , gManCaptureDirections =  [PLeft, Forward, PRight]
                      , gKingCaptureDirections = [Backward, PLeft, Forward, PRight]
                      , gKingSimpleMoveDirections = [Backward, PLeft, Forward, PRight]
                      , gManCaptures = manCaptures this
                      , gManCaptures1 = manCaptures1 this
                      , gCaptureMax = True
                      , gRemoveCapturedImmediately = True
                    }
  in  rules

turkish :: Turkish
turkish = Turkish $
  let rules = turkishBase rules
  in  rules

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = manCaptures1 rules ct
      -- when last horizontal reached, pass non-promoted piece to
      -- next moves check; man can not capture backward.
      nextMoves pm = genericNextMoves rules ct False pm
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

manCaptures1 :: GenericRules -> CaptureState -> [Capture]
manCaptures1 rules ct@(CaptureState {..}) =
    concatMap (check ctCurrent) $ filter allowedDir (gManCaptureDirections rules)
  where
    side = pieceSide ctPiece

    allowedDir dir =
      case ctPrevDirection of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case myNeighbour rules side dir a of
        Just victimAddr | not (aLabel victimAddr `LS.member` ctCaptured) ->
          case getPiece victimAddr ctBoard of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case myNeighbour rules side dir victimAddr of
                       Nothing -> []
                       Just freeAddr ->
                        if isFree freeAddr ctBoard || aLabel freeAddr `LS.member` ctCaptured
                          then let captured' = LS.insert (aLabel victimAddr) ctCaptured
                                   next = ct {
                                            ctPrevDirection = Just dir,
                                            ctCaptured = captured',
                                            ctCurrent = freeAddr
                                          }
                               in [Capture {
                                  cSrc = a,
                                  cDirection = dir,
                                  cInitSteps = 0,
                                  cFreeSteps = 1,
                                  cVictim = victimAddr,
                                  cRemoveVictimImmediately = gRemoveCapturedImmediately rules,
                                  cDst = freeAddr,
                                  cPromote = isLastHorizontal side freeAddr &&
                                             not (gCanCaptureFrom rules next)
                                }]
                          else []
        _ -> []

