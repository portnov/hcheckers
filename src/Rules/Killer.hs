{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Killer (Killer, killer, killerBase) where

import Data.Typeable
import Data.List (groupBy, sortOn, nub)
import Data.Maybe (mapMaybe)

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import Rules.International (internationalBase)

newtype Killer = Killer GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Killer where
  show = rulesName

instance HasTopology Killer where
  boardTopology _ = Diagonal

instance HasSideNotation Killer where
  sideNotation r = numericSideNotation (boardSize r)

instance HasBoardSize Killer where
  boardSize _ = (10, 10)

instance GameRules Killer where
  type EvaluatorForRules Killer = SimpleEvaluator
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

  rulesName _ = "killer"

  updateRules r _ = r

  getGameResult = genericGameResult

  possibleMoves (Killer rules) side board = gPossibleMoves rules side board
  hasCapturesOrPromotions (Killer rules) side board = genericHasCapturesOrPromotions rules side board
  mobilityScore (Killer rules) side board = gMobilityScore rules side board
  isManBlockedByKing = genericIsManBlockedByKing

  pdnId _ = "45"
  getAllAddresses r = addresses10 r


killerBase :: GenericRules -> GenericRules
killerBase =
  let rules this = internationalBase this {
                gKingCaptures = kingCaptures this
              }
  in rules

killer :: Killer
killer = Killer $
  let rules = killerBase rules
  in  rules

kingCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = gPieceCaptures1 rules ct
      grouped = groupBy (\c1 c2 -> cDirection c1 == cDirection c2) $ sortOn cDirection captures
      capturesByDirection = [(cDirection (head cs), cs) | cs <- grouped]
      nextMoves pm = genericNextMoves rules ct False pm 
  in nub $ concat $ flip map capturesByDirection $ \(dir, captures) ->
            let captures' = mapMaybe prune captures
                moves1 c = translateCapture ctPiece c
                allNext c = map nextMoves (moves1 c)
                isLast c = all null (allNext c)
                isCaptureKing c = getPiece (cVictim c) ctBoard == Just (Piece King (opposite side))
                prune c
                  | isCaptureKing c && isLast c && cFreeSteps c > 1 = Nothing
                  | otherwise = Just c
            in  if all isLast captures'
                  then concatMap moves1 captures'
                  else [catPMoves move1 move2 | c <- captures', move1 <- moves1 c, move2 <- nextMoves move1]

