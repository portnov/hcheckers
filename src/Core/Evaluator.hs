{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Evaluator where

import Data.Int
import Data.Aeson
import Data.Aeson.Types (parseMaybe)

import Core.Types
import Core.Board

data SimpleEvaluator = SimpleEvaluator {
    seRules :: SomeRules,
    seUsePositionalScore :: Bool,
    seMobilityWeight :: ScoreBase,
    seCenterWeight :: ScoreBase,
    seOppositeSideWeight :: ScoreBase,
    seBackedWeight :: ScoreBase,
    seAsymetryWeight :: ScoreBase,
    seKingCoef :: ScoreBase,
    seHelpedKingCoef :: ScoreBase
  }
  deriving (Show)

defaultEvaluator :: GameRules rules => rules -> SimpleEvaluator
defaultEvaluator rules =
  SimpleEvaluator {
      seRules = SomeRules rules
    , seUsePositionalScore = True
    , seMobilityWeight = 4
    , seCenterWeight = 2
    , seOppositeSideWeight = 3
    , seBackedWeight = 2
    , seAsymetryWeight = 1
    , seKingCoef = 3
    , seHelpedKingCoef = 5
  }

instance Evaluator SimpleEvaluator where
  evaluatorName _ = "simple"

  updateEval e (Object v) =
    case parseMaybe (.: "use_positional_score") v of
      Nothing -> e
      Just Nothing -> e
      Just (Just True) -> e {seUsePositionalScore = True}
      Just (Just False) -> e {seUsePositionalScore = False}

  evalBoard (SimpleEvaluator {seRules=SomeRules rules, ..}) whoAsks whoMovesNext board =
    let kingCoef side =
          -- King is much more useful when there are enough men to help it
          let (men, _) = myCounts side board
          in  if men > 3
                then seHelpedKingCoef
                else seKingCoef
        
        numericScore side =
          let (myMen, myKings) = myCounts side board
          in  kingCoef side * fromIntegral myKings + fromIntegral myMen

        myNumeric = numericScore whoAsks
        opponentNumeric = numericScore (opposite whoAsks)

        (nrows,ncols) = bSize board
        crow = nrows `div` 2
        ccol = ncols `div` 2
        halfCol = ccol `div` 2
        halfRow = crow `div` 2

        isCenter (Label col row) =
            (col >= ccol - halfCol && col < ccol + halfCol) &&
            (row >= crow - halfRow && row < crow + halfRow)

        isLeftHalf (Label col _) = col >= ccol

        asymetry side =
          let (leftMen, leftKings) = myLabelsCount side board isLeftHalf
              (rightMen, rightKings) = myLabelsCount side board (not . isLeftHalf)
          in  abs $ (leftMen + leftKings) - (rightMen + rightKings)

        isBackedAt addr side dir =
          case getNeighbourPiece (myDirection rules side dir) addr board of
            Nothing -> False
            Just p -> pieceSide p == side

        backedScoreOf side addr = 
          length $ filter (isBackedAt addr side) [BackwardLeft, BackwardRight]

        backedScore side =
          sum $ map (backedScoreOf side) $ allMyAddresses side board

        tempNumber side (Label col row)
          | col == 0 || col == ncols-1 = 0
          | otherwise =
              case boardSide (boardOrientation rules) side of
                Top -> nrows - row
                Bottom -> row + 1

        -- opponentSideCount :: Side -> Int
        opponentSideCount side =
          let (men, kings) = myLabelsCount' side board (tempNumber side)
          in  men

        positionalScore side =
          let (men, kings) = myLabelsCount side board isCenter
          in  if seUsePositionalScore
                then
                  seCenterWeight * (kingCoef side * fromIntegral kings + fromIntegral men) +
                  seOppositeSideWeight * fromIntegral (opponentSideCount side) +
                  seMobilityWeight * mobility side +
                  seBackedWeight * fromIntegral (backedScore side) -
                  seAsymetryWeight * fromIntegral (asymetry side)
                else 0

        mobility side = fromIntegral $ length (possibleMoves rules side board)

        myScore = Score myNumeric
                        (positionalScore whoAsks)
        opponentScore = Score opponentNumeric
                              (positionalScore (opposite whoAsks))

    in  if myNumeric == 0
          then loose
          else if opponentNumeric == 0
                 then win
                 else (myScore - opponentScore)

-- data ComplexEvaluator rules = ComplexEvaluator {
--     ceRules :: rules
--   , ceCaptureManCoef :: Score
--   , ceCaptureKingCoef :: Score
--   }
--   deriving (Eq, Show)
-- 
-- instance GameRules rules => Evaluator (ComplexEvaluator rules) where
--   evaluatorName _ = "complex"
-- 
--   evalBoard ce whoAsks whoMovesNext board =
--     let rules = ceRules ce
--         allMyMoves = possibleMoves rules whoAsks board
--         allOpponentMoves = possibleMoves rules (opposite whoAsks) board
-- 
--         myMoves = if whoAsks == whoMovesNext
--                     then allMyMoves
--                     else filter (not . isCapture) allMyMoves
--         opponentMoves = if whoAsks == whoMovesNext
--                           then filter (not . isCapture) allOpponentMoves
--                           else allOpponentMoves
-- 
--         (myMen, myKings) = myCounts whoAsks board
--         (opponentMen, opponentKings) = myCounts (opposite whoAsks) board
-- 
--     in  if (myMen == 0 && myKings == 0) || null allMyMoves
--           then {- trace (printf "Side %s loses" (show whoAsks)) -} (-win)
--           else if (opponentMen == 0 && opponentKings == 0) || null allOpponentMoves
--                  then {-  trace (printf "Side %s wins" (show whoAsks)) -} win
--                  else let movesScore s ms = if all isCapture ms
--                                                then let (men, kings) = unzip [capturesCounts rules move board | move <- ms]
--                                                         maxMen = if null men then 0 else maximum men
--                                                         maxKings = if null kings then 0 else maximum kings
--                                                     in  fromIntegral $
-- --                                                         trace (printf "Side %s possible captures: %s men, %s kings" (show s) (show men) (show kings)) $
--                                                         (ceCaptureManCoef ce) * fromIntegral maxMen + (ceCaptureKingCoef ce) * fromIntegral maxKings
--                                                else fromIntegral $ length ms
--                           myMovesScore = movesScore whoAsks myMoves
--                           opponentMovesScore = movesScore (opposite whoAsks) opponentMoves
--                       in --  trace (printf "Side %s moves score %d, opponent moves score %d, total score = %d" (show whoAsks) myMovesScore opponentMovesScore (myMovesScore - opponentMovesScore)) $
--                           (myMovesScore - opponentMovesScore)
-- 
