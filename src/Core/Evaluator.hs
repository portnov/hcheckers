
module Core.Evaluator where

import Core.Types
import Core.Board

win :: Score
win = max_score

max_score :: Score
max_score = 1000000

data SimpleEvaluator = SimpleEvaluator {
    seKingCoef :: Int
  }
  deriving (Eq, Show)

defaultEvaluator :: SimpleEvaluator
defaultEvaluator = SimpleEvaluator 5

instance Evaluator SimpleEvaluator where
  evaluatorName _ = "simple"

  evalBoard (SimpleEvaluator {seKingCoef=kingCoef}) whoAsks whoMovesNext board =
    let (myMen, myKings) = myCounts whoAsks board
        (opponentMen, opponentKings) = myCounts (opposite whoAsks) board
        myScore = kingCoef * myKings + myMen
        opponentScore = kingCoef * opponentKings + opponentMen
    in  if myMen == 0 && myKings == 0
          then -win
          else if opponentMen == 0 && opponentKings == 0
                 then win
                 else fromIntegral $ myScore - opponentScore

data ComplexEvaluator rules = ComplexEvaluator {
    ceRules :: rules
  , ceCaptureManCoef :: Int
  , ceCaptureKingCoef :: Int
  }
  deriving (Eq, Show)

instance GameRules rules => Evaluator (ComplexEvaluator rules) where
  evaluatorName _ = "complex"

  evalBoard ce whoAsks whoMovesNext board =
    let rules = ceRules ce
        allMyMoves = possibleMoves rules whoAsks board
        allOpponentMoves = possibleMoves rules (opposite whoAsks) board

        myMoves = if whoAsks == whoMovesNext
                    then allMyMoves
                    else filter (not . isCapture) allMyMoves
        opponentMoves = if whoAsks == whoMovesNext
                          then filter (not . isCapture) allOpponentMoves
                          else allOpponentMoves

    in  if null allMyMoves
          then {- trace (printf "Side %s loses" (show whoAsks)) -} (-win)
          else if null allOpponentMoves
                 then {-  trace (printf "Side %s wins" (show whoAsks)) -} win
                 else let movesScore s ms = if all isCapture ms
                                               then let (men, kings) = unzip [capturesCounts rules move board | move <- ms]
                                                        maxMen = if null men then 0 else maximum men
                                                        maxKings = if null kings then 0 else maximum kings
                                                    in  fromIntegral $
--                                                         trace (printf "Side %s possible captures: %s men, %s kings" (show s) (show men) (show kings)) $
                                                        (ceCaptureManCoef ce) * maxMen + (ceCaptureKingCoef ce) * maxKings
                                               else fromIntegral $ length ms
                          myMovesScore = movesScore whoAsks myMoves
                          opponentMovesScore = movesScore (opposite whoAsks) opponentMoves
                      in --  trace (printf "Side %s moves score %d, opponent moves score %d, total score = %d" (show whoAsks) myMovesScore opponentMovesScore (myMovesScore - opponentMovesScore)) $
                          myMovesScore - opponentMovesScore

