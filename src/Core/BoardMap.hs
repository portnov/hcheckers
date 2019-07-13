{-# LANGUAGE BangPatterns #-}
module Core.BoardMap where

import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Hashable
import qualified StmContainers.Map as SM
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
                      bcFirstMen = IS.size $ bFirstMen board
                    , bcFirstKings = IS.size $ bFirstKings board
                    , bcSecondMen = IS.size $ bSecondMen board
                    , bcSecondKings = IS.size $ bSecondKings board
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

insertBoardKey :: Address -> Piece -> BoardKey -> BoardKey
insertBoardKey a p bk = IM.insert (aIndex a) p bk

removeBoardKey :: Address -> Piece -> BoardKey -> BoardKey
removeBoardKey a p bk = IM.delete (aIndex a) bk

insertBoard :: Address -> Piece -> Board -> Board
insertBoard a p@(Piece Man First) b = b {
    bFirstMen = insertLabelSet (aLabel a) (bFirstMen b),
    bOccupied = insertLabelSet (aLabel a) (bOccupied b)
    -- boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece Man Second) b = b {
    bSecondMen = insertLabelSet (aLabel a) (bSecondMen b),
    bOccupied = insertLabelSet (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece King First) b = b {
    bFirstKings = insertLabelSet (aLabel a) (bFirstKings b),
    bOccupied = insertLabelSet (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece King Second) b = b {
    bSecondKings = insertLabelSet (aLabel a) (bSecondKings b),
    bOccupied = insertLabelSet (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }

removeBoard :: Address -> Piece -> Board -> Board
removeBoard a p@(Piece Man First) b = b {
    bFirstMen = deleteLabelSet (aLabel a) (bFirstMen b),
    bOccupied = deleteLabelSet (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece Man Second) b = b {
    bSecondMen = deleteLabelSet (aLabel a) (bSecondMen b),
    bOccupied = deleteLabelSet (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece King First) b = b {
    bFirstKings = deleteLabelSet (aLabel a) (bFirstKings b),
    bOccupied = deleteLabelSet (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece King Second) b = b {
    bSecondKings = deleteLabelSet (aLabel a) (bSecondKings b),
    bOccupied = deleteLabelSet (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }

newTBoardMap :: IO (TBoardMap a)
newTBoardMap = atomically SM.new

putBoardMap :: TBoardMap a -> Board -> a -> IO ()
putBoardMap bmap board value = atomically $ do
  SM.insert value (boardHash board) bmap
--   mbByHash <- SM.lookup (boardCounts board) bmap
--   case mbByHash of
--     Just byHash -> SM.insert value (boardHash board) byHash
--     Nothing -> do
--       byHash <- SM.new
--       SM.insert value (boardHash board) byHash
--       SM.insert byHash (boardCounts board) bmap

putBoardMapWith :: TBoardMap a -> (a -> a -> a) -> Board -> a -> IO ()
putBoardMapWith bmap plus board value = atomically $ do
    mbOld <- SM.lookup (boardHash board) bmap
    case mbOld of
      Nothing -> SM.insert value (boardHash board) bmap
      Just old -> SM.insert (plus old value) (boardHash board) bmap
--   mbByHash <- SM.lookup (boardCounts board) bmap
--   byHash <- case mbByHash of
--               Nothing -> SM.new
--               Just byHash -> return byHash
--   mbOld <- SM.lookup (boardHash board) byHash
--   case mbOld of
--     Nothing -> SM.insert value (boardHash board) byHash
--     Just old -> SM.insert (plus old value) (boardHash board) byHash

lookupBoardMap :: TBoardMap a -> Board -> IO (Maybe a)
lookupBoardMap bmap board = atomically $ do
  SM.lookup (boardHash board) bmap
--   mbByHash <- SM.lookup (boardCounts board) bmap
--   case mbByHash of
--     Nothing -> return Nothing
--     Just byHash -> SM.lookup (boardHash board) byHash

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

labelIndex :: Label -> FieldIndex
labelIndex (Label col row) = mkIndex col row

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

instance Show Board where
  show b = printf "{First Men: %s; Second Men: %s; First Kings: %s; Second Kings: %s}"
              (show $ labelSetToList $ bFirstMen b)
              (show $ labelSetToList $ bSecondMen b)
              (show $ labelSetToList $ bFirstKings b)
              (show $ labelSetToList $ bSecondKings b)

