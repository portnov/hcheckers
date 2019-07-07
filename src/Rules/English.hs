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

instance Show English where
  show = rulesName

instance HasTopology English where
  boardTopology _ = Diagonal

instance SimpleEvaluatorSupport English

instance GameRules English where
  boardSize _ = boardSize Russian.russian

  initBoard rnd r = 
    let board = buildBoard rnd r (boardOrientation r) (boardSize r)
        labels1 = line1labels ++ line2labels ++ line3labels
        labels2 = line8labels ++ line7labels ++ line6labels
    in  setManyPieces' labels1 (Piece Man Second) $ setManyPieces' labels2 (Piece Man First) board

  initPiecesCount _ = 24

  boardNotation r = numericNotation (boardSize r)
  parseNotation r = parseNumericNotation (boardSize r)

  dfltEvaluator r = SomeEval $ (defaultEvaluator r) {seKingCoef = 2, seHelpedKingCoef = 3}

  rulesName _ = "english"
  updateRules r _ = r
  getGameResult = genericGameResult

  possibleMoves (English rules) side board = gPossibleMoves rules side board
  mobilityScore (English rules) side board = gMobilityScore rules side board

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

kingSimpleMoves :: GenericRules -> Side -> Board -> Address -> [PossibleMove]
kingSimpleMoves rules side board src =
    concatMap check (gKingSimpleMoveDirections rules)
  where
    piece = Piece King side

    check dir =
      case myNeighbour rules side dir src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [PossibleMove {
                             pmBegin = src,
                             pmEnd = dst,
                             pmVictims = [],
                             pmVictimsCount = 0,
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
                                                  cRemoveVictimImmediately = gRemoveCapturedImmediately rules,
                                                  cDst = freeAddr,
                                                  cPromote = isLastHorizontal side freeAddr
                                                }]
                                          else []
        _ -> []

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = gManCaptures1 rules ct
      -- when last horizontal reached, pass non-promoted piece
      -- to next moves check; man cant capture backwards, so
      -- the piece will stop there.
      nextMoves pm = genericNextMoves rules ct False pm
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
      -- king cant be promoted anyway
      nextMoves pm = genericNextMoves rules ct False pm
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = translateCapture ctPiece capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catPMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

