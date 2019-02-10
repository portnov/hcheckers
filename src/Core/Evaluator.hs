{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Evaluator
  ( SimpleEvaluator (..),
    defaultEvaluator
  ) where

import           Data.Aeson
import           Data.Aeson.Types               ( parseMaybe )
import           Data.Default

import           Core.Types
import           Core.Board

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
defaultEvaluator rules = SimpleEvaluator
  { seRules              = SomeRules rules
  , seUsePositionalScore = True
  , seMobilityWeight     = 2
  , seCenterWeight       = 4
  , seOppositeSideWeight = 3
  , seBackedWeight       = 2
  , seAsymetryWeight     = 1
  , seKingCoef           = 3
  , seHelpedKingCoef     = 5
  }

data PreScore = PreScore {
      psNumeric :: ScoreBase
    , psMobility :: ScoreBase
    , psCenter :: ScoreBase
    , psTemp :: ScoreBase
    , psBacked :: ScoreBase
    , psAsymetry :: ScoreBase
  }

sub :: PreScore -> PreScore -> PreScore
sub ps1 ps2 = PreScore
  { psNumeric  = psNumeric ps1 - psNumeric ps2
  , psMobility = psMobility ps1 - psMobility ps2
  , psCenter   = psCenter ps1 - psCenter ps2
  , psTemp     = psTemp ps1 - psTemp ps2
  , psBacked   = psBacked ps1 - psBacked ps2
  , psAsymetry = psAsymetry ps1 - psAsymetry ps2
  }

instance Default PreScore where
  def = PreScore {
            psNumeric = 0
          , psMobility = 0
          , psCenter = 0
          , psTemp = 0
          , psBacked = 0
          , psAsymetry = 0
        }

preEval :: SimpleEvaluator -> Side -> Board -> PreScore
preEval (SimpleEvaluator { seRules = SomeRules rules, ..}) side board =
  let
    kingCoef =
      -- King is much more useful when there are enough men to help it
      let (men, _) = myCounts side board
      in  if men > 3 then seHelpedKingCoef else seKingCoef

    numericScore =
      let (myMen, myKings) = myCounts side board
      in  kingCoef * fromIntegral myKings + fromIntegral myMen

    (nrows, ncols) = bSize board
    crow           = nrows `div` 2
    ccol           = ncols `div` 2
    halfCol        = ccol `div` 2
    halfRow        = crow `div` 2

    isCenter (Label col row) =
      (col >= ccol - halfCol && col < ccol + halfCol)
        && (row >= crow - halfRow && row < crow + halfRow)

    isLeftHalf (Label col _) = col >= ccol

    asymetry =
      let (leftMen , leftKings ) = myLabelsCount side board isLeftHalf
          (rightMen, rightKings) = myLabelsCount side board (not . isLeftHalf)
      in  abs $ (leftMen + leftKings) - (rightMen + rightKings)

    isBackedAt addr dir =
      case getNeighbourPiece (myDirection rules side dir) addr board of
        Nothing -> False
        Just p  -> pieceSide p == side

    backedScoreOf addr =
      length $ filter (isBackedAt addr) [BackwardLeft, BackwardRight]

    backedScore =
      fromIntegral $ sum $ map backedScoreOf $ allMyAddresses side board

    tempNumber (Label col row)
      | col == 0 || col == ncols - 1 = 0
      | otherwise = case boardSide (boardOrientation rules) side of
        Top    -> nrows - row
        Bottom -> row + 1

    -- opponentSideCount :: Side -> Int
    opponentSideCount =
      let (men, kings) = myLabelsCount' side board tempNumber in men

    mobility = length (possibleMoves rules side board)

    centerScore =
      let (men, kings) = myLabelsCount side board isCenter
      in  kingCoef * fromIntegral kings + fromIntegral men
  in
    PreScore
      { psNumeric  = numericScore
      , psMobility = 0 -- fromIntegral mobility
      , psCenter   = centerScore
      , psTemp     = fromIntegral opponentSideCount
      , psBacked   = fromIntegral backedScore
      , psAsymetry = fromIntegral asymetry
      }

preEvalBoth :: SimpleEvaluator -> Board -> PreScore
preEvalBoth eval board =
  preEval eval First board `sub` preEval eval Second board

instance Evaluator SimpleEvaluator where
  evaluatorName _ = "simple"

  updateEval e (Object v) =
    case parseMaybe (.: "use_positional_score") v of
      Nothing -> e
      Just Nothing -> e
      Just (Just True) -> e {seUsePositionalScore = True}
      Just (Just False) -> e {seUsePositionalScore = False}

  evalBoard eval@(SimpleEvaluator {..}) whoAsks board =
    let ps1 = preEval eval whoAsks board
        ps2 = preEval eval (opposite whoAsks) board

        positionalScore ps =
          if seUsePositionalScore
            then
              seCenterWeight * psCenter ps +
              seOppositeSideWeight * psTemp ps +
              seMobilityWeight * psMobility ps +
              seBackedWeight * psBacked ps +
              seAsymetryWeight * psAsymetry ps
            else 0

        myNumeric = psNumeric ps1
        opponentNumeric = psNumeric ps2

        myScore = Score myNumeric (positionalScore ps1)
        opponentScore = Score opponentNumeric (positionalScore ps2)

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
