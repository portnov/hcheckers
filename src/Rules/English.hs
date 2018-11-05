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
  evalBoard rules = evalBoard defaultEvaluator

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

captures1 :: GenericRules -> Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PlayerDirection] -> [Capture]
captures1 rules mbPrevDir captured piece@(Piece _ side) board src dirs =
    concatMap (check src) $ filter allowedDir dirs
  where

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection rules side dir) a of
        Just victimAddr | not (aLabel victimAddr `labelSetMember` captured) ->
          case getPiece victimAddr board of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection rules side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr -> if isFree freeAddr board
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

manCaptures :: GenericRules -> Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove]
manCaptures rules mbPrevDir captured piece@(Piece _ side) board src =
  let captures = gManCaptures1 rules mbPrevDir captured piece board src
      nextMoves pm = gManCaptures rules (Just $ firstMoveDirection m) captured piece b (pmEnd pm)
                      where
                        m = pmMove pm
                        piece' = if pmPromote pm then promotePiece piece else piece
                        b = setPiece (pmEnd pm) piece' board
                        captured' = foldr insertLabelSet captured (map aLabel $ pmVictims pm)
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture piece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

kingCaptures1 :: GenericRules -> Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [Capture]
kingCaptures1 rules mbPrevDir captured piece board src =
  captures1 rules mbPrevDir captured piece board src (gKingCaptureDirections rules)

kingCaptures :: GenericRules -> Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove]
kingCaptures rules mbPrevDir captured piece@(Piece _ side) board src =
  let captures = gKingCaptures1 rules mbPrevDir captured piece board src
      nextMoves pm = gKingCaptures rules (Just $ firstMoveDirection m) captured' piece b (pmEnd pm)
                      where
                        m = pmMove pm
                        b = setPiece (pmEnd pm) piece board
                        captured' = foldr insertLabelSet captured (map aLabel $ pmVictims pm)
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = translateCapture piece capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catPMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

