
module Core.Evaluator where

import Data.Int

import Core.Types
import Core.Board

win :: Score
win = Score maxBound maxBound

loose :: Score
loose = Score (-maxBound) (-maxBound)

sub :: Score -> Score -> Score
sub s1 s2 = Score (sNumerical s1 - sNumerical s2) (sPositional s1 - sPositional s2)

data SimpleEvaluator = SimpleEvaluator {
    seKingCoef :: Int8
  }
  deriving (Eq, Show)

defaultEvaluator :: SimpleEvaluator
defaultEvaluator = SimpleEvaluator 5

instance Evaluator SimpleEvaluator where
  evaluatorName _ = "simple"

  evalBoard (SimpleEvaluator {seKingCoef=kingCoef}) whoAsks whoMovesNext board =
    let numericScore side =
          let (myMen, myKings) = myCounts side board
          in  kingCoef * fromIntegral myKings + fromIntegral myMen

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

        positionalScore side =
          let (men, kings) = myLabelsCount side board isCenter
          in  kingCoef * fromIntegral kings + fromIntegral men

        myScore = Score myNumeric (positionalScore whoAsks)
        opponentScore = Score opponentNumeric $ positionalScore (opposite whoAsks)

    in  if myNumeric == 0
          then loose
          else if opponentNumeric == 0
                 then win
                 else (myScore `sub` opponentScore)

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
