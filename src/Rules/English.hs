{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Rules.English (English (..), english) where

import Data.Typeable
import Data.List

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Evaluator
import qualified Rules.Russian as Russian
import Rules.Generic

newtype English = English GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Evaluator English where
  evaluatorName _ = "english"
  -- evalBoard rules = evalBoard $ ComplexEvaluator {ceRules = rules, ceCaptureManCoef = 10, ceCaptureKingCoef = 20}
  evalBoard rules = evalBoard $ defaultEvaluator {seKingCoef = 2}

instance Show English where
  show = rulesName

instance GameRules English where
  boardSize _ = boardSize Russian.russian

  initBoard r = 
    let board = buildBoard (boardOrientation r) (boardSize r)
        labels1 = line1labels ++ line2labels ++ line3labels
        labels2 = line8labels ++ line7labels ++ line6labels
    in  setManyPieces' labels1 (Piece Man Second) $ setManyPieces' labels2 (Piece Man First) board

  boardNotation r = numericNotation (boardSize r)
  parseNotation r = parseNumericNotation (boardSize r)

  rulesName _ = "english"
  updateRules r _ = r
  getGameResult = genericGameResult

  possibleMoves (English rules) side board = gPossibleMoves rules side board

  pdnId _ = "21"

english :: English
english = English $
  let rules = (abstractRules rules) {
                gKingSimpleMoves = kingSimpleMoves rules,
                gManCaptures = manCaptures rules,
                gKingCaptures = kingCaptures rules,
                gKingCaptures1 = kingCaptures1 rules,
                gManCaptureDirections = [ForwardLeft, ForwardRight],
                gBoardOrientation = SecondAtBottom
              }
  in  rules

kingSimpleMoves :: GenericRules -> Piece -> Board -> Address -> [PossibleMove]
kingSimpleMoves rules piece@(Piece _ side) board src =
    concatMap check (gKingSimpleMoveDirections rules)
  where
    check dir =
      case myNeighbour rules side dir src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [PossibleMove {
                             pmBegin = src,
                             pmEnd = dst,
                             pmVictims = [],
                             pmMove = Move src [Step dir False False],
                             pmPromote = False,
                             pmResult = [Take src, Put dst piece]
                            }]
                      else []

captures1 :: GenericRules -> CaptureState -> [PlayerDirection] -> [Capture]
captures1 rules ct@(CaptureState {..}) dirs =
    concatMap (check ctCurrent) $ filter allowedDir dirs
  where
    side = pieceSide ctPiece

    allowedDir dir =
      case ctPrevDirection of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection rules side dir) a of
        Just victimAddr | not (aLabel victimAddr `labelSetMember` ctCaptured) ->
          case getPiece victimAddr ctBoard of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection rules side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr -> if isFree freeAddr ctBoard
                                          then [Capture {
                                                  cSrc = a,
                                                  cDirection = dir,
                                                  cInitSteps = 0,
                                                  cFreeSteps = 1,
                                                  cVictim = victimAddr,
                                                  cDst = freeAddr,
                                                  cPromote = isLastHorizontal side freeAddr
                                                }]
                                          else []
        _ -> []

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = gManCaptures1 rules ct
      nextMoves pm = gManCaptures rules $ CaptureState
                                            (Just $ firstMoveDirection m)
                                            captured' ctPiece b (pmEnd pm)
                      where
                        m = pmMove pm
                        piece' = if pmPromote pm then promotePiece ctPiece else ctPiece
                        b = setPiece (pmEnd pm) piece' ctBoard
                        captured' = foldr insertLabelSet ctCaptured (map aLabel $ pmVictims pm)
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

kingCaptures1 :: GenericRules -> CaptureState -> [Capture]
kingCaptures1 rules ct =
  captures1 rules ct (gKingCaptureDirections rules)

kingCaptures :: GenericRules -> CaptureState -> [PossibleMove]
kingCaptures rules ct@(CaptureState {..}) =
  let captures = gKingCaptures1 rules ct
      nextMoves pm = gKingCaptures rules $ CaptureState
                                             (Just $ firstMoveDirection m)
                                             captured' ctPiece b (pmEnd pm)
                      where
                        m = pmMove pm
                        b = setPiece (pmEnd pm) ctPiece ctBoard
                        captured' = foldr insertLabelSet ctCaptured (map aLabel $ pmVictims pm)
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = translateCapture ctPiece capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catPMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

