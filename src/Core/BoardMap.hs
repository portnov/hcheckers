{-# LANGUAGE BangPatterns #-}
module Core.BoardMap where

import Control.Monad
import Control.Exception (bracket_)
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.HashMap.Strict as H
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Array.IArray as A
import Data.Hashable
import qualified STMContainers.Map as SM
import Data.Store
import Data.Word
import Text.Printf

import Core.Types

boxPiece :: UnboxedPiece -> Maybe Piece
boxPiece 0 = Nothing
boxPiece 1 = Just $ Piece Man First
boxPiece 2 = Just $ Piece Man Second
boxPiece 3 = Just $ Piece King First
boxPiece 4 = Just $ Piece King Second

unboxPiece :: Maybe Piece -> UnboxedPiece
unboxPiece Nothing = 0
unboxPiece (Just (Piece Man First)) = 1
unboxPiece (Just (Piece Man Second)) = 2
unboxPiece (Just (Piece King First)) = 3
unboxPiece (Just (Piece King Second)) = 4

calcBoardCounts :: Board -> BoardCounts
calcBoardCounts board = BoardCounts {
                      bcFirstMen = IS.size $ bkFirstMen bk
                    , bcFirstKings = IS.size $ bkFirstKings bk
                    , bcSecondMen = IS.size $ bkSecondMen bk
                    , bcSecondKings = IS.size $ bkSecondKings bk
                  }
  where
    bk = boardKey board

insertBoardCounts :: Piece -> BoardCounts -> BoardCounts
insertBoardCounts p !bc =
  case p of
    Piece Man First -> bc {bcFirstMen = bcFirstMen bc + 1}
    Piece Man Second -> bc {bcSecondMen = bcSecondMen bc + 1}
    Piece King First -> bc {bcFirstKings = bcFirstKings bc + 1}
    Piece King Second -> bc {bcSecondKings = bcSecondKings bc + 1}

removeBoardCounts :: Piece -> BoardCounts -> BoardCounts
removeBoardCounts p !bc =
  case p of
    Piece Man First -> bc {bcFirstMen = bcFirstMen bc - 1}
    Piece Man Second -> bc {bcSecondMen = bcSecondMen bc - 1}
    Piece King First -> bc {bcFirstKings = bcFirstKings bc - 1}
    Piece King Second -> bc {bcSecondKings = bcSecondKings bc - 1}

insertBoardKey :: Address -> Piece -> BoardKey -> BoardKey
insertBoardKey a (Piece Man First) !bk = bk {bkFirstMen = insertLabelSet (aLabel a) (bkFirstMen bk)}
insertBoardKey a (Piece Man Second) !bk = bk {bkSecondMen = insertLabelSet (aLabel a) (bkSecondMen bk)}
insertBoardKey a (Piece King First) !bk = bk {bkFirstKings = insertLabelSet (aLabel a) (bkFirstKings bk)}
insertBoardKey a (Piece King Second) !bk = bk {bkSecondKings = insertLabelSet (aLabel a) (bkSecondKings bk)}

removeBoardKey :: Address -> Piece -> BoardKey -> BoardKey
removeBoardKey a (Piece Man First) !bk = bk {bkFirstMen = deleteLabelSet (aLabel a) (bkFirstMen bk)}
removeBoardKey a (Piece Man Second) !bk = bk {bkSecondMen = deleteLabelSet (aLabel a) (bkSecondMen bk)}
removeBoardKey a (Piece King First) !bk = bk {bkFirstKings = deleteLabelSet (aLabel a) (bkFirstKings bk)}
removeBoardKey a (Piece King Second) !bk = bk {bkSecondKings = deleteLabelSet (aLabel a) (bkSecondKings bk)}

newTBoardMap :: IO (TBoardMap a)
newTBoardMap = atomically $ SM.new

putBoardMap :: TBoardMap a -> Board -> a -> IO ()
putBoardMap bmap board value = atomically $
  SM.insert value board bmap

putBoardMapWith :: TBoardMap a -> (a -> a -> a) -> Board -> a -> IO ()
putBoardMapWith bmap plus board value = atomically $ do
  mbOld <- SM.lookup board bmap
  case mbOld of
    Nothing -> SM.insert value board bmap
    Just old -> SM.insert (plus old value) board bmap

lookupBoardMap :: TBoardMap a -> Board -> IO (Maybe a)
lookupBoardMap bmap board = atomically $
  SM.lookup board bmap

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

