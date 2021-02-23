{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Simple (Simple, simple) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Evaluator
import qualified Rules.Russian as Russian
import Rules.Generic

newtype Simple = Simple GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Simple where
  show = rulesName

instance SimpleEvaluatorSupport Simple where
  getAllAddresses r = addresses8 r

instance HasTopology Simple where
  boardTopology _ = Diagonal

instance GameRules Simple where
  type EvaluatorForRules Simple = SimpleEvaluator
  initBoard rnd _ = initBoard rnd Russian.russian
  boardSize _ = boardSize Russian.russian
  initPiecesCount _ = 24

  boardNotation _ = boardNotation Russian.russian

  parseNotation _ = parseNotation Russian.russian

  dfltEvaluator r = defaultEvaluator r

  rulesName _ = "simple"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (Simple rules) side board = gPossibleMoves rules side board
  mobilityScore (Simple rules) side board = gMobilityScore rules side board

  pdnId _ = "43"

simple :: Simple
simple = Simple $
  let rules = (Russian.russianBase rules) {
                gManSimpleMoves = manSimpleMoves rules,
                gManCaptures = manCaptures rules,
                gManCaptures1 = manCaptures1 rules
              }
  in  rules

manSimpleMoves :: GenericRules -> Side -> Board -> Address -> [PossibleMove]
manSimpleMoves rules side board src =
    concatMap check (gManSimpleMoveDirections rules)
  where
    piece = Piece Man side
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

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let captures = gManCaptures1 rules ct
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
      case neighbour (myDirection rules side dir) a of
        Just victimAddr | not (aLabel victimAddr `labelSetMember` ctCaptured) ->
          case getPiece victimAddr ctBoard of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection rules side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr ->
                        if isFree freeAddr ctBoard
                          then [Capture {
                                  cSrc = a,
                                  cDirection = dir,
                                  cInitSteps = 0,
                                  cVictim = victimAddr,
                                  cRemoveVictimImmediately = gRemoveCapturedImmediately rules,
                                  cFreeSteps = 1,
                                  cDst = freeAddr,
                                  cPromote = False
                                }]
                          else []
        _ -> []

