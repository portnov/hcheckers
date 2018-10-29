{-# LANGUAGE DeriveDataTypeable #-}
module Rules.English (English (..)) where

import Data.Typeable
import Data.List

import Core.Types
import Core.Board
import Core.Evaluator
import qualified Rules.Russian as Russian

data English = English
  deriving (Show, Eq, Ord, Typeable)

instance Evaluator English where
  evaluatorName _ = "english"
  evalBoard rules = evalBoard $ ComplexEvaluator {ceRules = rules, ceCaptureManCoef = 10, ceCaptureKingCoef = 20}

instance GameRules English where
  boardSize English = boardSize Russian.Russian

  initBoard English = 
    let board = buildBoard (boardOrientation English) (boardSize English)
        labels1 = line1labels ++ line2labels ++ line3labels
        labels2 = line8labels ++ line7labels ++ line6labels
    in  setManyPieces' labels1 (Piece Man Second) $ setManyPieces' labels2 (Piece Man First) board

  boardNotation English = numericNotation (boardSize English)
  parseNotation English = parseNumericNotation (boardSize English)

  rulesName English = "english"
  updateRules English _ = English
  getGameResult = genericGameResult

  boardOrientation English = SecondAtBottom

  possibleMoves English side board = 
    let simpleMoves = concatMap (possibleSimpleMoves1 English board) (allMyAddresses side board)
        captures = concatMap (possibleCaptures1 English board) (allMyAddresses side board)
    in  if null captures
          then simpleMoves
          else captures

possibleSimpleMoves1 :: GameRules rules => rules -> Board -> Address -> [Move]
possibleSimpleMoves1 rules board src =
  case getPiece src board of
    Nothing -> error "possibleSimpleMoves1: not my field"
    Just piece@(Piece Man _) -> Russian.manSimpleMoves rules piece board src
    Just piece@(Piece King _) -> kingSimpleMoves rules piece board src

kingSimpleMoves :: GameRules rules => rules -> Piece -> Board -> Address -> [Move]
kingSimpleMoves rules piece@(Piece _ side) board src =
    concatMap check [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    check dir =
      case neighbour (myDirection rules side dir) src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [Move src [Step dir False False]]
                      else []

possibleCaptures1 :: GameRules rules => rules -> Board -> Address -> [Move]
possibleCaptures1 rules board src =
  case getPiece src board of
    Nothing -> error $ "possibleCaptures: not my field"
    Just piece@(Piece Man _) -> manCaptures rules Nothing piece board src
    Just piece@(Piece King _) -> kingCaptures rules Nothing piece board src

manCaptures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Russian.Capture]
manCaptures1 rules mbPrevDir piece board src =
  captures1 rules mbPrevDir piece board src [ForwardLeft, ForwardRight]

captures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [PlayerDirection] -> [Russian.Capture]
captures1 rules mbPrevDir piece@(Piece _ side) board src dirs =
    concatMap (check src) $ filter allowedDir dirs
  where

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection rules side dir) a of
        Nothing -> []
        Just victimAddr ->
          case getPiece victimAddr board of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection rules side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr -> if isFree freeAddr board
                                          then [Russian.Capture {
                                                  Russian.cSrc = a,
                                                  Russian.cDirection = dir,
                                                  Russian.cInitSteps = 0,
                                                  Russian.cFreeSteps = 1,
                                                  -- cDst = freeAddr,
                                                  Russian.cPromote = isLastHorizontal side freeAddr
                                                }]
                                          else []

manCaptures :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures rules mbPrevDir piece@(Piece _ side) board src =
  let captures = manCaptures1 rules mbPrevDir piece board src
      nextMoves m = manCaptures rules (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove rules side m board
  in concat $ flip map captures $ \capture ->
       let [move1] = Russian.translateCapture side board capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

kingCaptures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Russian.Capture]
kingCaptures1 rules mbPrevDir piece board src =
  captures1 rules mbPrevDir piece board src [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]

kingCaptures :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
kingCaptures rules mbPrevDir piece@(Piece _ side) board src =
  let captures = kingCaptures1 rules mbPrevDir piece board src
      nextMoves m = kingCaptures rules (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove rules side m board
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = Russian.translateCapture side board capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

