{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Rules.International (International (..)) where

import Data.Typeable
import Data.List

import Core.Types
import Core.Board
import qualified Rules.Russian as Russian

-- import Debug.Trace

data International = International
  deriving (Show, Eq, Ord, Typeable)

instance GameRules International where
  boardSize International = (10, 10)

  initBoard International =
    let board = buildBoard (10, 10)
        labels1 = ["a1", "c1", "e1", "g1", "i1",
                   "b2", "d2", "f2", "h2", "j2",
                   "a3", "c3", "e3", "g3", "i3",
                   "b4", "d4", "f4", "h4", "j4"]

        labels2 = ["b10", "d10", "f10", "h10", "j10",
                   "a9", "c9", "e9", "g9", "i9",
                   "b8", "d8", "f8", "h8", "j8",
                   "a7", "c7", "e7", "g7", "i7"]

    in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

  boardNotation International = numericNotation (boardSize International)

  parseNotation International = parseNumericNotation (boardSize International)

  rulesName International = "international"

  updateRules International _ = International

  getGameResult = genericGameResult

  possibleMoves International side board =
    let simpleMoves = concatMap (Russian.possibleSimpleMoves1 International board) (allMyAddresses side board)
        captures = concatMap (possibleCaptures1 board) (allMyAddresses side board)
    in  if null captures
          then simpleMoves
          else let captures' = sortOn (negate . capturesCount) captures
                   n = capturesCount (head captures')
               in  filter (\c -> capturesCount c == n) captures'

possibleCaptures1 :: Board -> Address -> [Move]
possibleCaptures1 board src =
  case getPiece src board of
    Nothing -> error $ "possibleCaptures: not my field"
    Just piece@(Piece Man _) -> manCaptures International Nothing piece board src
    Just piece@(Piece King _) -> Russian.kingCaptures International Nothing piece board src

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

canCaptureFrom :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> Bool
canCaptureFrom rules mbPrevDir piece@(Piece Man side) board src =
    null (manCaptures1 rules mbPrevDir piece board src)
canCaptureFrom rules mbPrevDir piece@(Piece King side) board src =
    null (Russian.kingCaptures1 rules mbPrevDir piece board src)

manCaptures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Russian.Capture]
manCaptures1 rules mbPrevDir piece@(Piece _ side) board src =
    concatMap (check src) $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
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
                       Just freeAddr ->
                        if isFree freeAddr board
                          then [Russian.Capture {
                                  Russian.cSrc = a,
                                  Russian.cDirection = dir,
                                  Russian.cInitSteps = 0,
                                  Russian.cFreeSteps = 1,
                                  Russian.cPromote = isLastHorizontal side freeAddr && not (canCaptureFrom rules (Just dir) piece board freeAddr)
                                }]
                          else []

