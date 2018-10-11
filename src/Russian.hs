module Russian where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf

import Types
import Board

import Debug.Trace

data Russian = Russian
  deriving (Show)

instance GameRules Russian where
  initBoard Russian = board8

  possibleMoves Russian side board =
    let all = concatMap (possibleMoves1 side board) (allMyAddresses side board)
        (captures, moves) = partition isCapture all
    in  if null captures
          then moves
          else let captures' = sortOn (negate . capturesCount) captures
                   n = capturesCount (head captures')
               in  filter (\c -> capturesCount c == n) captures'

  updateRules Russian _ = Russian


possibleMoves1 :: Side -> Board -> Address -> [Move]
possibleMoves1 side board src =
  case getPiece src board of
    Nothing -> error $ "possibleMoves1: not my field"
    Just (Piece Man _) -> manMoves side board src
    Just (Piece King _) -> kingMoves side board src

manMoves :: Side -> Board -> Address -> [Move]
manMoves side board src =
  let captures = manCaptures (Piece Man side) board src
      moves = manSimpleMoves side board src
  in  captures ++ moves

manSimpleMoves :: Side -> Board -> Address -> [Move]
manSimpleMoves side board src = check ForwardLeft ++ check ForwardRight
  where
    piece = getPiece_ "manSimpleMoves" src board

    check dir =
      let move = simpleMove side src dir
      in  if isWellFormedMove piece board move
            then [move]
            else []

captures1 :: Piece -> Board -> Address -> [Move]
captures1 piece board src =
  case piece of
    (Piece Man _) -> manCaptures1 piece board src
    (Piece King _) -> kingCaptures1 piece board src

manCaptures :: Piece -> Board -> Address -> [Move]
manCaptures piece@(Piece _ side) board src =
  let moves = captures1 piece board src
      nextMoves m = captures1 p b a
                      where (b, a, p) = applyMove side m board
  in concat $ flip map moves $ \move1 ->
       let moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

manCaptures1 :: Piece -> Board -> Address -> [Move]
manCaptures1 (Piece _ side) board src = concatMap (check src) [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    piece = getPiece_ "manCaptures1" src board

    check a dir =
      let move = simpleCapture side a dir
      in  if isWellFormedMove piece board move
            then [move]
            else []

kingCaptures :: Piece -> Board -> Address -> [Move]
kingCaptures piece@(Piece _ side) board src =
  let moves = captures1 piece board src
      nextMoves m = captures1 p b a
                      where (b, a, p) = applyMove side m board
  in nub $ concat $ flip map moves $ \move1 ->
       let moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

kingCaptures1 :: Piece -> Board -> Address -> [Move]
kingCaptures1 piece@(Piece _ side) board src = concatMap check [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    
    check dir =
      case search dir src of
        Nothing -> []
        Just steps ->
          let maxFree = freeFieldsCount dir (length steps) 1
              freeSteps = tail $ inits $ replicate maxFree (Step dir False False)
          in {- trace (show steps) $ -} [Move src (steps ++ free) | free <- freeSteps]

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
    check dir = takeWhile (isWellFormedMove piece board) $ map (kingMove side src dir) [1 ..]
    piece = getPiece_ "kingSimpleMoves" src board

kingMoves :: Side -> Board -> Address -> [Move]
kingMoves side board src =
  kingCaptures (Piece King side) board src ++ kingSimpleMoves side board src

win :: Integer
win = 1000000

captureManCoef :: Int
captureManCoef = 10

captureKingCoef :: Int
captureKingCoef = 50

instance Evaluator Russian where
  evalBoard rules side board =
    let moves = possibleMoves rules side board
        opponentMoves = possibleMoves rules (opposite side) board
    in  if null moves
          then {-trace (printf "Side %s loses" (show side))-} (-win)
          else if null opponentMoves
                 then {-trace (printf "Side %s wins" (show side))-} win
                 else let movesScore s ms = if all isCapture ms
                                               then let (men, kings) = unzip [capturesCounts move board | move <- ms]
                                                    in  fromIntegral $
                                                        -- trace (printf "Side %s possible captures: %s men, %s kings" (show s) (show men) (show kings)) $
                                                        captureManCoef * sum men + captureKingCoef * sum kings
                                               else fromIntegral $ length ms
                          myMovesScore = movesScore side moves
                          opponentMovesScore = movesScore (opposite side) opponentMoves
                      in  -- trace (printf "Side %s moves score %d, opponent moves score %d, total score = %d" (show side) myMovesScore opponentMovesScore (myMovesScore - opponentMovesScore)) $
                          myMovesScore - opponentMovesScore

