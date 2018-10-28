
module Core.BoardMap where

import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import Data.Array

import Core.Types

calcBoardCounts :: Board -> BoardCounts
calcBoardCounts board = BoardCounts {
                      bcFirstMen = count (Piece Man First)
                    , bcFirstKings = count (Piece King First)
                    , bcSecondMen = count (Piece Man Second)
                    , bcSecondKings = count (Piece King Second)
                  }
  where
    count piece = countAddresses (== piece) $ bPieces board

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
    labels piece = findLabels (== piece) $ bPieces board

putBoardMap :: Board -> a -> BoardMap a -> BoardMap a
putBoardMap board x bmap =
    M.unionWith H.union bmap init
  where
    init = M.singleton (boardCounts board) $ H.singleton (boardKey board) x

putBoardMapWith :: (a -> a -> a) -> (BoardCounts,BoardKey) -> a -> BoardMap a -> BoardMap a
putBoardMapWith plus (bc,bk) x bmap =
    M.unionWith (H.unionWith plus) bmap init
  where
    init = M.singleton bc $ H.singleton bk x

lookupBoardMap :: (BoardCounts,BoardKey) -> BoardMap a -> Maybe a
lookupBoardMap (bc,bk) bmap =
  H.lookup bk =<< M.lookup bc bmap

singleBoardMap :: Board -> a -> BoardMap a
singleBoardMap board x =
  M.singleton (boardCounts board) $ H.singleton (boardKey board) x

emptyBoardMap :: BoardMap a
emptyBoardMap = M.empty

deleteBoardMap :: BoardCounts -> BoardKey -> BoardMap a -> BoardMap a
deleteBoardMap bc bk bm =
  let del m = let m' = H.delete bk m
              in  if H.null m'
                    then Nothing
                    else Just m'
  in  M.update del bc bm

------------------

unpackIndex :: FieldIndex -> Label
unpackIndex (col,row) = Label col row

aIndex :: Address -> FieldIndex
aIndex a = (labelColumn l, labelRow l)
  where l = aLabel a

buildLabelMap :: Line -> Line -> [(Label, a)] -> LabelMap a
buildLabelMap nrows ncols pairs =
  array ((0,0), (ncols-1,nrows-1))
        [((col, row), value) | (Label col row, value) <- pairs]

lookupLabel :: Label -> LabelMap a -> Maybe a
lookupLabel (Label col row) lmap = Just $ lmap ! (col,row)

emptyAddressMap :: BoardSize -> AddressMap a
emptyAddressMap (nrows,ncols) =
  listArray ((0,0), (ncols-1,nrows-1)) $ replicate (fromIntegral $ ncols*nrows) Nothing

lookupAddress :: Address -> AddressMap a -> Maybe a
lookupAddress a amap = amap ! aIndex a

setAddress :: Address -> a -> AddressMap a -> AddressMap a
setAddress a x amap = amap // [(aIndex a, Just x)]

removeAddress :: Address -> AddressMap a -> AddressMap a
removeAddress a amap = amap // [(aIndex a, Nothing)]

findLabels :: (a -> Bool) -> AddressMap a -> [Label]
findLabels fn amap = [unpackIndex idx | (idx, Just value) <- assocs amap, fn value]

countAddresses :: (a -> Bool) -> AddressMap a -> Int
countAddresses fn amap = length $ findLabels fn amap

occupiedLabels :: AddressMap a -> [(Label, a)]
occupiedLabels amap = [(unpackIndex idx, value) | (idx, Just value) <- assocs amap]

labelMapKeys :: LabelMap a -> [Label]
labelMapKeys lmap = map unpackIndex $ indices lmap

