{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module International (International (..)) where

import Data.Typeable
import Data.List

import Types
import Board
import qualified Russian

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
    let simpleMoves = concatMap (Russian.possibleSimpleMoves1 board) (allMyAddresses side board)
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
    Just piece@(Piece Man _) -> manCaptures Nothing piece board src
    Just piece@(Piece King _) -> Russian.kingCaptures Nothing piece board src

manCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures mbPrevDir piece@(Piece _ side) board src =
  let captures = manCaptures1 mbPrevDir piece board src
      nextMoves m = manCaptures (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in concat $ flip map captures $ \capture ->
       let [move1] = Russian.translateCapture side board capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

canCaptureFrom :: Maybe PlayerDirection -> Piece -> Board -> Address -> Bool
canCaptureFrom mbPrevDir piece@(Piece Man side) board src = null (manCaptures1 mbPrevDir piece board src)
canCaptureFrom mbPrevDir piece@(Piece King side) board src = null (Russian.kingCaptures1 mbPrevDir piece board src)

manCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Russian.Capture]
manCaptures1 mbPrevDir piece@(Piece _ side) board src = concatMap (check src) $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection side dir) a of
        Nothing -> []
        Just victimAddr ->
          case getPiece victimAddr board of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr ->
                        if isFree freeAddr board
                          then [Russian.Capture {
                                  Russian.cSrc = a,
                                  Russian.cDirection = dir,
                                  Russian.cInitSteps = 0,
                                  Russian.cFreeSteps = 1,
                                  Russian.cPromote = isLastHorizontal side freeAddr && not (canCaptureFrom (Just dir) piece board freeAddr)
                                }]
                          else []

