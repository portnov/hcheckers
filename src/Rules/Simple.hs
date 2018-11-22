{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

instance Evaluator Simple where
  evaluatorName _ = "simple"
  evalBoard _ = evalBoard defaultEvaluator

instance Show Simple where
  show = rulesName

instance GameRules Simple where
  initBoard _ = initBoard Russian.russian
  boardSize _ = boardSize Russian.russian

  boardNotation _ = boardNotation Russian.russian

  parseNotation _ = parseNotation Russian.russian

  rulesName _ = "simple"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (Simple rules) side board = gPossibleMoves rules side board

  pdnId _ = "43"

simple :: Simple
simple = Simple $
  let rules = (Russian.russianBase rules) {
                gManSimpleMoves = manSimpleMoves rules,
                gManCaptures = manCaptures rules,
                gManCaptures1 = manCaptures1 rules
              }
  in  rules

manSimpleMoves :: GenericRules -> Piece -> Board -> Address -> [PossibleMove]
manSimpleMoves rules piece@(Piece _ side) board src =
    concatMap check (gManSimpleMoveDirections rules)
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

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let captures = gManCaptures1 rules ct
      nextMoves pm = gManCaptures rules $ ct {
                                            ctPrevDirection = Just (firstMoveDirection m),
                                            ctCaptured = captured',
                                            ctBoard = b,
                                            ctCurrent = pmEnd pm
                                          }
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
                                  cFreeSteps = 1,
                                  cDst = freeAddr,
                                  cPromote = False
                                }]
                          else []
        _ -> []

