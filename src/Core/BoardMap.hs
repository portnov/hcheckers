
module Core.BoardMap where

import Control.Monad
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Hashable
import Data.Store
import Data.List
import Data.Array
import Data.Word
import Text.Printf

import Core.Types

calcBoardCounts :: Board -> BoardCounts
calcBoardCounts board = BoardCounts {
                      bcFirstMen = IS.size (bFirstMen board)
                    , bcFirstKings = IS.size (bFirstKings board)
                    , bcSecondMen = IS.size (bSecondMen board)
                    , bcSecondKings = IS.size (bSecondKings board)
                  }

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
                   bkFirstMen = bFirstMen board
                 , bkFirstKings = bFirstKings board
                 , bkSecondMen = bSecondMen board
                 , bkSecondKings = bSecondKings board
                }

insertBoardKey :: Address -> Piece -> BoardKey -> BoardKey
insertBoardKey a (Piece Man First) bk = bk {bkFirstMen = insertLabelSet (aLabel a) (bkFirstMen bk)}
insertBoardKey a (Piece Man Second) bk = bk {bkSecondMen = insertLabelSet (aLabel a) (bkSecondMen bk)}
insertBoardKey a (Piece King First) bk = bk {bkFirstKings = insertLabelSet (aLabel a) (bkFirstKings bk)}
insertBoardKey a (Piece King Second) bk = bk {bkSecondKings = insertLabelSet (aLabel a) (bkSecondKings bk)}

removeBoardKey :: Address -> Piece -> BoardKey -> BoardKey
removeBoardKey a (Piece Man First) bk = bk {bkFirstMen = deleteLabelSet (aLabel a) (bkFirstMen bk)}
removeBoardKey a (Piece Man Second) bk = bk {bkSecondMen = deleteLabelSet (aLabel a) (bkSecondMen bk)}
removeBoardKey a (Piece King First) bk = bk {bkFirstKings = deleteLabelSet (aLabel a) (bkFirstKings bk)}
removeBoardKey a (Piece King Second) bk = bk {bkSecondKings = deleteLabelSet (aLabel a) (bkSecondKings bk)}

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

boardMapSize :: BoardMap a -> Int
boardMapSize bmap = sum $ map H.size $ M.elems bmap

------------------

unpackIndex :: FieldIndex -> Label
unpackIndex n =
  let col = n `div` 16
      row = n `mod` 16
  in  Label (fromIntegral col) (fromIntegral row)

aIndex :: Address -> FieldIndex
aIndex a = fromIntegral (labelColumn l) * 16 + fromIntegral (labelRow l)
  where l = aLabel a

mkIndex :: Line -> Line -> FieldIndex
mkIndex col row =  fromIntegral col * 16 + fromIntegral row

buildLabelMap :: Line -> Line -> [(Label, a)] -> LabelMap a
buildLabelMap nrows ncols pairs =
  IM.fromList [(mkIndex col row, value) |  (Label col row, value) <- pairs]

lookupLabel :: Label -> LabelMap a -> Maybe a
lookupLabel (Label col row) lmap = IM.lookup (mkIndex col row) lmap

emptyAddressMap :: BoardSize -> AddressMap a
emptyAddressMap (nrows,ncols) = IM.empty

lookupAddress :: Address -> AddressMap a -> Maybe a
lookupAddress a amap = IM.lookup (aIndex a) amap

setAddress :: Address -> a -> AddressMap a -> AddressMap a
setAddress a x amap = IM.insert (aIndex a) x amap

removeAddress :: Address -> AddressMap a -> AddressMap a
removeAddress a amap = IM.delete (aIndex a) amap

addressMapContains :: Label -> AddressMap a -> Bool
addressMapContains (Label col row) amap = IM.member (mkIndex col row) amap

findLabels :: (a -> Bool) -> AddressMap a -> [Label]
findLabels fn amap = [unpackIndex idx | idx <- IM.keys $ IM.filter fn amap]

countAddresses :: (a -> Bool) -> AddressMap a -> Int
countAddresses fn amap = length $ findLabels fn amap

occupiedLabels :: AddressMap a -> [(Label, a)]
occupiedLabels amap = [(unpackIndex idx, value) | (idx, value) <- IM.assocs amap]

labelMapKeys :: LabelMap a -> [Label]
labelMapKeys lmap = map unpackIndex $ IM.keys lmap

--------------------

emptyLabelSet :: LabelSet
emptyLabelSet = IS.empty

labelSetToList :: LabelSet -> [Label]
labelSetToList set = map unpackIndex $ IS.toList set

labelSetFromList :: [Label] -> LabelSet
labelSetFromList list = IS.fromList [mkIndex col row | Label col row <- list]

insertLabelSet :: Label -> LabelSet -> LabelSet
insertLabelSet (Label col row) set = IS.insert (mkIndex col row) set

deleteLabelSet :: Label -> LabelSet -> LabelSet
deleteLabelSet (Label col row) set = IS.delete (mkIndex col row) set

labelSetMember :: Label -> LabelSet -> Bool
labelSetMember (Label col row) set = IS.member (mkIndex col row) set

instance Hashable IS.IntSet where
  hashWithSalt salt set = hashWithSalt salt (IS.toList set)

instance Store BoardKey where
  poke bk = do
    poke (fromIntegral (IS.size (bkFirstMen bk)) :: Word8)
    forM_ (labelSetToList $ bkFirstMen bk) poke
    poke (fromIntegral (IS.size (bkSecondMen bk)) :: Word8)
    forM_ (labelSetToList $ bkSecondMen bk) poke
    poke (fromIntegral (IS.size (bkFirstKings bk)) :: Word8)
    forM_ (labelSetToList $ bkFirstKings bk) poke
    poke (fromIntegral (IS.size (bkSecondKings bk)) :: Word8)
    forM_ (labelSetToList $ bkSecondKings bk) poke

  peek = do
    n <- peek :: Peek Word8
    firstMen <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    secondMen <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    firstKings <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    secondKings <- replicateM (fromIntegral n) peek
    return $ BoardKey
              (labelSetFromList firstMen)
              (labelSetFromList secondMen)
              (labelSetFromList firstKings)
              (labelSetFromList secondKings)

  size = VarSize $ \bk ->
         4 + IS.size (bkFirstMen bk) +
             IS.size (bkSecondMen bk) +
             IS.size (bkFirstKings bk)  +
             IS.size (bkSecondKings bk) 
  
instance Hashable BoardKey where
  hashWithSalt salt bk =
    salt `hashWithSalt` bkFirstMen bk `hashWithSalt` bkSecondMen bk `hashWithSalt` bkFirstKings bk `hashWithSalt` bkSecondKings bk

instance Show BoardKey where
  show bk = printf "{First Men: %s; Second Men: %s; First Kings: %s; Second Kings: %s}"
              (show $ labelSetToList $ bkFirstMen bk)
              (show $ labelSetToList $ bkSecondMen bk)
              (show $ labelSetToList $ bkFirstKings bk)
              (show $ labelSetToList $ bkSecondKings bk)

instance Show Board where
  show board = show $ boardKey board

