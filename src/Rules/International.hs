{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Rules.International (International, international, internationalBase) where

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

instance Evaluator International where
  evaluatorName _ = "international"
  -- TODO: I suspect in international draughts a king has much more weight
  evalBoard _ = evalBoard defaultEvaluator

instance GameRules International where
  boardSize _ = (10, 10)

  initBoard r =
    let board = buildBoard (boardOrientation r) (10, 10)
        labels1 = ["a1", "c1", "e1", "g1", "i1",
                   "b2", "d2", "f2", "h2", "j2",
                   "a3", "c3", "e3", "g3", "i3",
                   "b4", "d4", "f4", "h4", "j4"]

        labels2 = ["b10", "d10", "f10", "h10", "j10",
                   "a9", "c9", "e9", "g9", "i9",
                   "b8", "d8", "f8", "h8", "j8",
                   "a7", "c7", "e7", "g7", "i7"]

    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  boardNotation r = numericNotation (boardSize r)

  parseNotation r = parseNumericNotation (boardSize r)

  rulesName _ = "international"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (International rules) side board = gPossibleMoves rules side board

  pdnId _ = "20"

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
      nextMoves pm = manCaptures rules $ CaptureState
                                          (Just $ firstMoveDirection m)
                                          captured' ctPiece b (pmEnd pm)
                      where
                        m = pmMove pm
                        b = setPiece (pmEnd pm) ctPiece ctBoard
                        captured' = foldr insertLabelSet ctCaptured (map aLabel $ pmVictims pm)
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

manCaptures1 :: GenericRules -> CaptureState -> [Capture]
manCaptures1 rules ct@(CaptureState {..}) =
    concatMap (check ctSource) $ filter allowedDir (gManCaptureDirections rules)
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
                                            ctSource = freeAddr
                                          }
                               in [Capture {
                                  cSrc = a,
                                  cDirection = dir,
                                  cInitSteps = 0,
                                  cFreeSteps = 1,
                                  cVictim = victimAddr,
                                  cDst = freeAddr,
                                  cPromote = isLastHorizontal side freeAddr &&
                                             not (gCanCaptureFrom rules next)
                                }]
                          else []
        _ -> []

