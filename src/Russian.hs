module Russian where

import Control.Monad
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Text.Printf

import Types
import Board

data Russian = Russian
  deriving (Show)

instance GameRules Russian where
  possibleMoves Russian side board =
    let all = concatMap (possibleMoves1 side board) (allMyAddresses side board)
        (captures, moves) = partition isCapture all
    in  if null captures
          then moves
          else let captures' = sortOn (negate . capturesCount) captures
                   n = capturesCount (head captures')
               in  filter (\c -> capturesCount c == n) captures'


possibleMoves1 :: Side -> Board -> Address -> [Move]
possibleMoves1 side board src =
  case getPiece src board of
    Nothing -> error $ "possibleMoves1: not my field"
    Just (Piece Man _) -> manMoves side board src
    Just (Piece King _) -> kingMoves side board src

manMoves :: Side -> Board -> Address -> [Move]
manMoves side board src =
  let captures = manCaptures side board src
      moves = manSimpleMoves side board src
  in  captures ++ moves

manSimpleMoves :: Side -> Board -> Address -> [Move]
manSimpleMoves side board src = check ForwardLeft ++ check ForwardRight
  where
    check dir =
      let move = simpleMove side src dir
      in  if isWellFormedMove side board move
            then [move]
            else []

manCaptures :: Side -> Board -> Address -> [Move]
manCaptures side board src = []

kingMoves :: Side -> Board -> Address -> [Move]
kingMoves side board src = []

