
module BoardMap where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.String
import Data.Char (isDigit, ord)
import Data.Aeson (Value)
import Text.Printf
import GHC.Generics
-- import qualified Data.IntTrie as IT

import Types

calcBoardCounts :: Board -> BoardCounts
calcBoardCounts board = BoardCounts {
                      bcFirstMen = count (Piece Man First)
                    , bcFirstKings = count (Piece King First)
                    , bcSecondMen = count (Piece Man Second)
                    , bcSecondKings = count (Piece King Second)
                  }
  where
    count piece = M.size $ M.filter (== piece) $ bPieces board

insertBoardCounts :: Piece -> BoardCounts -> BoardCounts
insertBoardCounts p bc =
  case p of
    Piece Man First -> bc {bcFirstMen = bcFirstMen bc + 1}
    Piece Man Second -> bc {bcSecondMen = bcSecondMen bc + 1}
    Piece King First -> bc {bcFirstKings = bcFirstKings bc + 1}
    Piece King Second -> bc {bcSecondKings = bcSecondKings bc + 1}

removeBoardCounts :: Piece -> BoardCounts -> BoardCounts
removeBoardCounts p bc =
  case p of
    Piece Man First -> bc {bcFirstMen = bcFirstMen bc - 1}
    Piece Man Second -> bc {bcSecondMen = bcSecondMen bc - 1}
    Piece King First -> bc {bcFirstKings = bcFirstKings bc - 1}
    Piece King Second -> bc {bcSecondKings = bcSecondKings bc - 1}

calcBoardKey :: Board -> BoardKey
calcBoardKey board = BoardKey {
                   bkFirstMen = labels (Piece Man First)
                 , bkFirstKings = labels (Piece King First)
                 , bkSecondMen = labels (Piece Man Second)
                 , bkSecondKings = labels (Piece King Second)
                }
  where
    labels piece = map aLabel $ M.keys $ M.filter (== piece) $ bPieces board

putBoardMap :: Board -> a -> BoardMap a -> BoardMap a
putBoardMap board x bmap =
    M.unionWith M.union bmap init
  where
    init = M.singleton (boardCounts board) $ M.singleton (boardKey board) x

lookupBoardMap :: Board -> BoardMap a -> Maybe a
lookupBoardMap board bmap =
  M.lookup (boardKey board) =<< M.lookup (boardCounts board) bmap

singleBoardMap :: Board -> a -> BoardMap a
singleBoardMap board x =
  M.singleton (boardCounts board) $ M.singleton (boardKey board) x

