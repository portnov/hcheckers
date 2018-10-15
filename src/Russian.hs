{-# LANGUAGE DeriveDataTypeable #-}
module Russian where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Typeable
import Data.Ord
import Data.List
import Text.Printf

import Types
import Board

import Debug.Trace

data Russian = Russian
  deriving (Show, Typeable)

instance GameRules Russian where
  initBoard Russian = board8

  rulesName Russian = "russian"

  possibleMoves Russian side board =
    let all = concatMap (possibleMoves1 side board) (allMyAddresses side board)
        (captures, moves) = partition isCapture all
    in  if null captures
          then moves
          else captures
--                let captures' = sortOn (negate . capturesCount) captures
--                    n = capturesCount (head captures')
--                in  filter (\c -> capturesCount c == n) captures'

  updateRules Russian _ = Russian


possibleMoves1 :: Side -> Board -> Address -> [Move]
possibleMoves1 side board src =
  case getPiece src board of
    Nothing -> error $ "possibleMoves1: not my field"
    Just (Piece Man _) -> manMoves side board src
    Just (Piece King _) -> kingMoves side board src

manMoves :: Side -> Board -> Address -> [Move]
manMoves side board src =
  let captures = manCaptures Nothing (Piece Man side) board src
      moves = manSimpleMoves side board src
  in  captures ++ moves

manSimpleMoves :: Side -> Board -> Address -> [Move]
manSimpleMoves side board src = check ForwardLeft ++ check ForwardRight
  where
    piece = getPiece_ "manSimpleMoves" src board

    check dir =
      let move = simpleMove side src dir
      in  case isWellFormedMove piece board move of
            ValidMove -> [move]
            _ -> []

captures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
captures1 mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures1 mbPrevDir piece board src
    (Piece King _) -> kingCaptures1 mbPrevDir piece board src

pieceCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
pieceCaptures mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures mbPrevDir piece board src
    (Piece King _) -> kingCaptures mbPrevDir piece board src

manCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures mbPrevDir piece@(Piece _ side) board src =
  let moves = captures1 mbPrevDir piece board src
      nextMoves m = pieceCaptures (Just $ captureDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in concat $ flip map moves $ \move1 ->
       let moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

captureDirection :: Move -> PlayerDirection
captureDirection move = sDirection $ head $ moveSteps move

manCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures1 mbPrevDir (Piece _ side) board src = concatMap (check src) $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    piece = getPiece_ "manCaptures1" src board

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      let move = simpleCapture side a dir
      in  case isWellFormedMove piece board move of
            ValidMove -> [move]
            e -> {- trace (printf "%s: %s: cant catpure to %s: %s" (show side) (show src) (show dir) (show e)) $-} []

kingCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
kingCaptures mbPrevDir piece@(Piece _ side) board src =
  let moves = captures1 mbPrevDir piece board src
      nextMoves m = pieceCaptures (Just $ captureDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in nub $ concat $ flip map moves $ \move1 ->
       let moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

kingCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
kingCaptures1 mbPrevDir piece@(Piece _ side) board src = concatMap check $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    
    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check dir =
      case search dir src of
        Nothing -> []
        Just steps ->
          let maxFree = freeFieldsCount dir (length steps) 1
              freeSteps = tail $ inits $ replicate maxFree (Step dir False False)
          in  if maxFree == 0
                then []
                else {- trace (show steps) $ -} [Move src (steps ++ free) | free <- freeSteps]

    search dir a =
      -- trace (printf "A: %s, dir: %s" (show a) (show dir)) $
      -- trace (printf "%s: %s" (show a) (show $ getPiece a board)) $
      case neighbour (myDirection side dir) a of
        Nothing -> Nothing
        Just a' -> case getPiece a' board of
                     Nothing -> case search dir a' of
                                  Nothing -> Nothing
                                  Just steps -> Just $ Step dir False False : steps
                     Just p -> if isOpponentPiece side p
                                 then Just [Step dir True False]
                                 else Nothing

--       if isOpponentAt side a board
--         then
--           if isJust (getNeighbourPiece (myDirection side dir) a board)
--             then trace "stop" Nothing
--             else trace "X" $ Just [Step dir True False]
--         else
--           case neighbour (myDirection side dir) a of
--             Nothing -> Nothing
--             Just a' -> case search dir a' of
--                          Nothing -> Nothing
--                          Just steps -> trace "+" $ Just $ (Step dir False False) : steps

    freeFieldsCount dir n k =
      -- trace (printf "Src: %s, dir: %s, n: %d, k: %d, piece: %s"
      --                 (show src) (show dir) n k (show $ getPieceInDirection (myDirection side dir) src board (n+k))) $
      if isFreeInDirection (myDirection side dir) src board (n+k)
        then freeFieldsCount dir n (k+1)
        else {- trace (printf "K: %s" (show k)) $ -} k-1

kingSimpleMoves :: Side -> Board -> Address -> [Move]
kingSimpleMoves side board src = concatMap check [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    check dir = takeWhile (\m -> isWellFormedMove piece board m == ValidMove) $ map (kingMove side src dir) [1 ..]
    piece = Piece King side

kingMoves :: Side -> Board -> Address -> [Move]
kingMoves side board src =
  kingCaptures Nothing (Piece King side) board src ++ kingSimpleMoves side board src

