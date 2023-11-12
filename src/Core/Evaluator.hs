{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
module Core.Evaluator
  ( SimpleEvaluator (..),
    SimpleEvaluatorData (..),
    weightForSide,
    defaultEvaluator,
    preEval
  ) where

import           Data.Aeson
import           Data.Aeson.Types as AT
import           Data.Default
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.IntSet as IS

import Core.Types
import Core.Board
import Core.BoardMap

data SimpleEvaluatorWeights = SimpleEvaluatorWeights {
    sewFirst :: {-# UNPACK #-} !ScoreBase
  , sewSecond :: {-# UNPACK #-} !ScoreBase
  }
  deriving (Show)

weightForSide :: Side -> SimpleEvaluatorWeights -> ScoreBase
weightForSide First w = sewFirst w
weightForSide Second w = sewSecond w

data SimpleEvaluatorData = SimpleEvaluatorData {  
    sedCenter :: SimpleEvaluatorWeights
  }
  deriving (Show)

data SimpleEvaluator = SimpleEvaluator {
    seRules :: SomeRules,
    seUsePositionalScore :: Bool,
    seMobilityWeight :: ScoreBase,
    seBackyardWeight :: ScoreBase,
    seCenterWeight :: ScoreBase,
    seOppositeSideWeight :: ScoreBase,
    seBorderManWeight :: ScoreBase,
    seBackedWeight :: ScoreBase,
    seAsymetryWeight :: ScoreBase,
    seTempAsymetryWeight :: ScoreBase,
    sePreKingWeight :: ScoreBase,
    seKingCoef :: ScoreBase,
    sePositionalKingWeight :: ScoreBase,
    seHelpedKingCoef :: ScoreBase,
    seThreatWeight :: ScoreBase,
    seAttackedManCoef :: ScoreBase,
    seAttackedKingCoef :: ScoreBase,
    seKingOnKeyFieldWeight :: ScoreBase,
    seManBlockedByKingWeight :: ScoreBase,
    seCache :: M.Map Address SimpleEvaluatorData
  }
  deriving (Show)

defaultEvaluator :: GameRules rules => rules -> SimpleEvaluator
defaultEvaluator rules = SimpleEvaluator
    { seRules              = iface
    , seUsePositionalScore = True
    , seMobilityWeight     = 30
    , seBackyardWeight     = 14
    , seCenterWeight       = 16
    , seOppositeSideWeight = 32
    , seBorderManWeight    = -16
    , seBackedWeight       = 24
    , seAsymetryWeight     = -12
    , seTempAsymetryWeight = -12
    , sePreKingWeight      = 28
    , seKingCoef           = 3
    , sePositionalKingWeight = 0
    , seHelpedKingCoef     = 5
    , seThreatWeight       = 10
    , seAttackedManCoef = -40
    , seAttackedKingCoef = -80
    , seKingOnKeyFieldWeight = 10
    , seManBlockedByKingWeight = -20
    , seCache = buildCache iface
    }
  where
    iface = SomeRules rules

parseEvaluator :: SimpleEvaluator -> Value -> AT.Parser SimpleEvaluator
parseEvaluator def = withObject "Evaluator" $ \v -> SimpleEvaluator
    <$> pure (seRules def)
    <*> v .:? "use_positional_score" .!= seUsePositionalScore def
    <*> v .:? "mobility_weight" .!= seMobilityWeight def
    <*> v .:? "backyard_weight" .!= seBackyardWeight def
    <*> v .:? "center_weight" .!= seCenterWeight def
    <*> v .:? "opposite_side_weight" .!= seOppositeSideWeight def
    <*> v .:? "border_man_weight" .!= seBorderManWeight def
    <*> v .:? "backed_weight" .!= seBackedWeight def
    <*> v .:? "asymetry_weight" .!= seAsymetryWeight def
    <*> v .:? "temp_asymetry_weight" .!= seTempAsymetryWeight def
    <*> v .:? "pre_king_weight" .!= sePreKingWeight def
    <*> v .:? "king_coef" .!= seKingCoef def
    <*> v .:? "positional_king_weight" .!= sePositionalKingWeight def
    <*> v .:? "helped_king_coef" .!= seHelpedKingCoef def
    <*> v .:? "threat_weight" .!= seThreatWeight def
    <*> v .:? "attacked_man_coef" .!= seAttackedManCoef def
    <*> v .:? "attacked_king_coef" .!= seAttackedKingCoef def
    <*> v .:? "king_on_key_field_weight" .!= seKingOnKeyFieldWeight def
    <*> v .:? "man_blocked_by_king_weight" .!= seManBlockedByKingWeight def
    <*> pure (seCache def)

instance ToJSON SimpleEvaluator where
  toJSON p = object [
      "use_positional_score" .= seUsePositionalScore p,
      "mobility_weight" .= seMobilityWeight p,
      "backyard_weight" .= seBackyardWeight p,
      "center_weight" .= seCenterWeight p,
      "opposite_side_weight" .= seOppositeSideWeight p,
      "border_man_weight" .= seBorderManWeight p,
      "backed_weight" .= seBackedWeight p,
      "asymetry_weight" .= seAsymetryWeight p,
      "temp_asymetry_weight" .= seTempAsymetryWeight p,
      "pre_king_weight" .= sePreKingWeight p,
      "king_coef" .= seKingCoef p,
      "positional_king_weight" .= sePositionalKingWeight p,
      "helped_king_coef" .= seHelpedKingCoef p,
      "threat_weight" .= seThreatWeight p,
      "attacked_man_coef" .= seAttackedManCoef p,
      "attacked_king_coef" .= seAttackedKingCoef p,
      "king_on_key_field_weight" .= seKingOnKeyFieldWeight p,
      "man_blocked_by_king_weight" .= seManBlockedByKingWeight p
    ]

data PreScore = PreScore {
      psNumeric :: !ScoreBase
    , psMobility :: ScoreBase
    , psBackyard :: ScoreBase
    , psCenter :: ScoreBase
    , psTemp :: ScoreBase
    , psBorder :: ScoreBase
    , psBacked :: ScoreBase
    , psAsymetry :: ScoreBase
    , psTempAsymetry :: ScoreBase
    , psPreKing :: ScoreBase
    , psPosKing :: ScoreBase
    , psAttackedMen :: ScoreBase
    , psAttackedKings :: ScoreBase
    , psThreats :: ScoreBase
    , psKingsOnKeyFields :: ScoreBase
    , psMenBlockedByKings :: ScoreBase
  }
  deriving (Show)

sub :: PreScore -> PreScore -> PreScore
sub ps1 ps2 = PreScore
  { psNumeric  = psNumeric ps1 - psNumeric ps2
  , psMobility = psMobility ps1 - psMobility ps2
  , psBackyard = psBackyard ps1 - psBackyard ps2
  , psCenter   = psCenter ps1 - psCenter ps2
  , psTemp     = psTemp ps1 - psTemp ps2
  , psBorder   = psBorder ps1 - psBorder ps2
  , psBacked   = psBacked ps1 - psBacked ps2
  , psAsymetry = psAsymetry ps1 - psAsymetry ps2
  , psTempAsymetry = psTempAsymetry ps1 - psTempAsymetry ps2
  , psPreKing  = psPreKing ps1 - psPreKing ps2
  , psPosKing  = psPosKing ps1 - psPosKing ps2
  , psAttackedMen = psAttackedMen ps1 - psAttackedMen ps2
  , psAttackedKings = psAttackedKings ps1 - psAttackedKings ps2
  , psThreats = psThreats ps1 - psThreats ps2
  , psKingsOnKeyFields = psKingsOnKeyFields ps1 - psKingsOnKeyFields ps2
  , psMenBlockedByKings = psMenBlockedByKings ps1 - psMenBlockedByKings ps2
  }

instance Default PreScore where
  def = PreScore {
            psNumeric = 0
          , psMobility = 0
          , psBackyard = 0
          , psCenter = 0
          , psTemp = 0
          , psBorder = 0
          , psBacked = 0
          , psAsymetry = 0
          , psTempAsymetry = 0
          , psPreKing = 0
          , psPosKing = 0
          , psAttackedMen = 0
          , psAttackedKings = 0
          , psThreats = 0
          , psKingsOnKeyFields = 0
          , psMenBlockedByKings = 0
        }

waveRho :: SomeRules -> Side -> (Address -> Bool) -> Address -> ScoreBase -> ScoreBase
waveRho (SomeRules rules) side isGood addr best = go addr
  where
    go :: Address -> ScoreBase
    go addr
      | isGood addr = best
      | otherwise =
        let check :: PlayerDirection -> ScoreBase
            check dir =
              case myNeighbour rules side dir addr of
                Nothing -> 0
                Just dst -> max 0 $ go dst - 1
        in  maximum $ map check $ getManSimpleMoveDirections rules

buildCache :: SomeRules -> M.Map Address SimpleEvaluatorData
buildCache iface@(SomeRules rules) = M.fromList [(addr, labelData addr) | addr <- getAllAddresses rules]
  where
    labelData addr = SimpleEvaluatorData $ SimpleEvaluatorWeights {
        sewFirst = waveRho iface First (isCenter . aLabel) addr best,
        sewSecond = waveRho iface Second (isCenter . aLabel) addr best
      }

    (nrows, ncols) = boardSize rules

    best = fromIntegral $ nrows `div` 2 - 1

    crow           = nrows `div` 2
    ccol           = ncols `div` 2
    halfCol        = ccol `div` 2
    halfRow        = crow `div` 2

    isCenter (Label col row) =
      (col >= ccol - halfCol && col < ccol + halfCol)
        && (row >= crow - 1 && row < crow + 1)
        -- && (row >= crow - halfRow && row < crow + halfRow)

preEval :: SimpleEvaluator -> Side -> Board -> PreScore
preEval (SimpleEvaluator { seRules = iface@(SomeRules rules), ..}) side board =
  let
    kingCoef = seKingCoef
      -- King is much more useful when there are enough men to help it
--       let (men, _) = myCounts side board
--       in  if men > 3 then seHelpedKingCoef else seKingCoef

    numericScore = fromIntegral $ length $ possibleMoves rules side board
    positionalKingScore = fromIntegral $ labelSetSize (myKingsS side board)

    (nrows, ncols) = bSize board
    crow           = nrows `div` 2
    ccol           = ncols `div` 2
    halfCol        = ccol `div` 2
    halfRow        = crow `div` 2

    isLeftHalf (Label col _) = col >= ccol

    asymetry =
      let (leftMen , leftKings ) = myLabelsCount side board isLeftHalf
          (rightMen, rightKings) = myLabelsCount side board (not . isLeftHalf)
      in  abs $ (leftMen + leftKings) - (rightMen + rightKings)

    tempAsymetry =
      let tempNumber' label
            | isLeftHalf label = tempNumber label
            | otherwise = negate (tempNumber label)
      in  abs $ fst $ myLabelsCount' side board tempNumber'

    isBackedAt addr dir =
      case myNeighbour rules side dir addr of
        Nothing -> True
        Just back -> isPieceAt back board side

    backedScoreOf addr =
      length $ filter (isBackedAt addr) $ getBackDirections rules

    backedScore =
      fromIntegral $ sum $ map backedScoreOf $ allMyAddresses side board

    isBackyard (Label _ row) =
        case boardSide (boardOrientation rules) side of
          Top    -> row == nrows-1
          Bottom -> row == 0

    backyard =
      let (backMen, _) = myLabelsCount side board isBackyard
      in  backMen

    tempNumber (Label col row) =
      case boardSide (boardOrientation rules) side of
        Top    -> nrows - row
        Bottom -> row + 1

    isBorder (Label col row) =
        col == 0 || col == ncols - 1

    borderNumber =
      let (men, _) = myLabelsCount side board isBorder
      in  men

    centerNumber addr = weightForSide side $ sedCenter $ seCache M.! addr

    opponentSideCount =
      sum $ map tempNumber $ myMen side board

    threatsBy addr = sum $ map check $ getForwardDirections rules
      where
        check dir =
          case myNeighbour rules side dir addr of
            Nothing -> 0
            Just f1 ->
              if isFree f1 board
                then case myNeighbour rules side dir f1 of
                       Nothing -> 0
                       Just f2 -> if isFree f2 board
                                    then 1
                                    else 0
                else 0

    threatsCount = sum $ map threatsBy $ myMenA side board

    isPreKing board src = any check $ getManSimpleMoveDirections rules
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> False
            Just dst -> isLastHorizontal side dst && isFree dst board

    preKings =
      length $ filter (isPreKing board) $ myMenA side board

    mobility = mobilityScore rules side board

    attackedFields = boardAttacked side board
    attackedMen = getPiecesCount (Piece Man side) attackedFields board
    attackedKings = getPiecesCount (Piece King side) attackedFields board

    centerScore =
      let (men, kings) = myAddressesCount' side board centerNumber in men + kings

    keyFields = kingKeyFields rules
    keyFieldsCnt = fromIntegral $ IS.size keyFields
    
    occupiedKeyFields =
      let otherPieces = IS.difference (bOccupied board) (myKingsS side board)
      in  fromIntegral $ IS.size $ IS.intersection otherPieces keyFields

    kingsAtKeyFields =
      fromIntegral $ IS.size $ IS.intersection (myKingsS side board) keyFields

    kingsKeyFieldsScore = kingsAtKeyFields * keyFieldsCnt `div` (occupiedKeyFields + 1)

    menBlockedByKings =
      let opponentKings = myKingsS (opposite side) board
          men = myMenS side board
          anyS p set = not $ IS.null $ IS.filter p set
      in  IS.size $ IS.filter (\manIdx -> anyS (\kingIdx -> isManBlockedByKing rules side board (unpackIndex manIdx) (unpackIndex kingIdx)) opponentKings) men

  in
    PreScore
      { psNumeric  = numericScore
      , psMobility = fromIntegral mobility
      , psCenter   = fromIntegral centerScore
      , psBackyard = fromIntegral backyard
      , psTemp     = fromIntegral opponentSideCount
      , psBorder   = fromIntegral borderNumber
      , psBacked   = fromIntegral backedScore
      , psAsymetry = fromIntegral asymetry
      , psTempAsymetry = fromIntegral tempAsymetry
      , psPreKing  = fromIntegral preKings
      , psPosKing  = fromIntegral positionalKingScore
      , psAttackedMen = fromIntegral attackedMen
      , psAttackedKings = fromIntegral attackedKings
      , psThreats = fromIntegral threatsCount
      , psKingsOnKeyFields = fromIntegral kingsKeyFieldsScore
      , psMenBlockedByKings = fromIntegral menBlockedByKings
      }

preEvalBoth :: SimpleEvaluator -> Board -> PreScore
preEvalBoth eval board =
  preEval eval First board `sub` preEval eval Second board

instance Evaluator SimpleEvaluator where
  evaluatorName _ = "simple"

  updateEval e v =
    case AT.parseMaybe (parseEvaluator e) v of
      Nothing -> e
      Just e' -> e'

  evalBoard eval@(SimpleEvaluator {seRules = SomeRules rules, ..}) whoAsks board =
    let ps1 = preEval eval whoAsks board
        ps2 = preEval eval (opposite whoAsks) board

        initCount = initPiecesCount rules
        openingCount = 2 * initCount `div` 3
        endgameCount = initCount `div` 3
--         midgameCount = initCount `div` 2
        count = totalCount board

        crescentAdjustment :: ScoreBase -> ScoreBase -> ScoreBase
        crescentAdjustment from to
          | count >= openingCount = to
          | count <= endgameCount = from
          | otherwise = (from - to) * fromIntegral (count - openingCount) `div` fromIntegral (endgameCount - openingCount) + to

        backyardWeight = crescentAdjustment seBackyardWeight (seBackyardWeight `div` 2)
        centerWeight = seCenterWeight -- crescentAdjustment seCenterWeight (seCenterWeight `div` 2)
        tempWeight = crescentAdjustment (seOppositeSideWeight * 2) seOppositeSideWeight 
        asymetryWeight = crescentAdjustment 0 seAsymetryWeight
        tempAsymetryWeight = crescentAdjustment 0 seTempAsymetryWeight

        positionalScore ps =
          if seUsePositionalScore
            then
              centerWeight * psCenter ps +
              backyardWeight * psBackyard ps +
              tempWeight * psTemp ps +
              seBorderManWeight * psBorder ps +
              seMobilityWeight * psMobility ps +
              seBackedWeight * psBacked ps +
              asymetryWeight * psAsymetry ps +
              tempAsymetryWeight * psTempAsymetry ps +
              sePreKingWeight * psPreKing ps +
              sePositionalKingWeight * psPosKing ps +
              seAttackedManCoef * psAttackedMen ps +
              seAttackedKingCoef * psAttackedKings ps +
              seThreatWeight * psThreats ps +
              seKingOnKeyFieldWeight * psKingsOnKeyFields ps +
              seManBlockedByKingWeight * psMenBlockedByKings ps
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

instance VectorEvaluator SimpleEvaluator where

  evalToVector (SimpleEvaluator {..}) = V.fromList $ map fromIntegral $ [
        seMobilityWeight, seBackyardWeight,
        seCenterWeight, seOppositeSideWeight,
        seBackedWeight,
        seAsymetryWeight, seTempAsymetryWeight, sePreKingWeight,
        seKingCoef, sePositionalKingWeight, seAttackedManCoef,
        seAttackedKingCoef, seBorderManWeight,
        seThreatWeight, seKingOnKeyFieldWeight,
        seManBlockedByKingWeight]

  evalFromVector rules v = SimpleEvaluator {
          seRules = iface
        , seUsePositionalScore = True
        , seMobilityWeight = round (v V.! 0)
        , seBackyardWeight = round (v V.! 1)
        , seCenterWeight = round (v V.! 2)
        , seOppositeSideWeight = round (v V.! 3)
        , seBackedWeight = round (v V.! 4)
        , seAsymetryWeight = round (v V.! 5)
        , seTempAsymetryWeight = round (v V.! 6)
        , sePreKingWeight = round (v V.! 7)
        , seKingCoef = round (v V.! 8)
        , sePositionalKingWeight = round (v V.! 9)
        , seHelpedKingCoef = round (v V.! 8)
        , seAttackedManCoef = round (v V.! 10)
        , seAttackedKingCoef = round (v V.! 11)
        , seBorderManWeight = round (v V.! 12)
        , seThreatWeight = round (v V.! 13)
        , seKingOnKeyFieldWeight = round (v V.! 14)
        , seManBlockedByKingWeight = round (v V.! 15)
        , seCache = buildCache iface
      }
    where
      iface = SomeRules rules
        

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
