{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.International (International, international, internationalBase, manCaptures) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Evaluator
import Rules.Generic

-- import Debug.Trace

newtype International = International GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show International where
  show = rulesName

instance HasTopology International where
  boardTopology _ = Diagonal

instance HasSideNotation International where
  sideNotation r = numericSideNotation (boardSize r)

instance GameRules International where
  type EvaluatorForRules International = SimpleEvaluator
  boardSize _ = (10, 10)
  kingKeyFields _ = mainDiagonal 10

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

  initPiecesCount _ = 40

  boardNotation r = numericNotation (boardSize r)

  dfltEvaluator r = (defaultEvaluator r) {seKingCoef = 5, seHelpedKingCoef = 6}

  parseNotation r = parseNumericNotation (boardSize r)

  rulesName _ = "international"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (International rules) side board = gPossibleMoves rules side board
  mobilityScore (International rules) side board = gMobilityScore rules side board

  pdnId _ = "20"
  getAllAddresses r = addresses10 r

internationalBase :: GenericRules -> GenericRules
internationalBase =
  let rules this = abstractRules this {
                gManCaptures = manCaptures this,
                gManCaptures1 = manCaptures1 this,
                gCaptureMax = True
              }
  in rules

international :: International
international = International $
  let rules = internationalBase rules
  in  rules

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = manCaptures1 rules ct
      -- when last horizontal reached, pass non-promoted piece to
      -- next moves check; man can capture backward, so it will
      -- continue capture as a man if it can.
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
        Just victimAddr | not (aLabel victimAddr `labelSetMember` ctCaptured) ->
          case getPiece victimAddr ctBoard of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case myNeighbour rules side dir victimAddr of
                       Nothing -> []
                       Just freeAddr ->
                        if isFree freeAddr ctBoard
                          then let captured' = insertLabelSet (aLabel victimAddr) ctCaptured
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

