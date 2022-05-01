{-# LANGUAGE BangPatterns #-}
module Core.BoardMap where

import Control.Concurrent.STM
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Hashable
import qualified StmContainers.Map as SM
import Text.Printf
import Foreign.Storable

import Core.Types
import qualified Core.HTable as HT
import Core.LabelSet as LS

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
                      bcFirstMen = LS.size $ bFirstMen board
                    , bcFirstKings = LS.size $ bFirstKings board
                    , bcSecondMen = LS.size $ bSecondMen board
                    , bcSecondKings = LS.size $ bSecondKings board
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
    bFirstMen = LS.insert (aLabel a) (bFirstMen b),
    bOccupied = LS.insert (aLabel a) (bOccupied b)
    -- boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece Man Second) b = b {
    bSecondMen = LS.insert (aLabel a) (bSecondMen b),
    bOccupied = LS.insert (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece King First) b = b {
    bFirstKings = LS.insert (aLabel a) (bFirstKings b),
    bOccupied = LS.insert (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }
insertBoard a p@(Piece King Second) b = b {
    bSecondKings = LS.insert (aLabel a) (bSecondKings b),
    bOccupied = LS.insert (aLabel a) (bOccupied b)
--     boardCounts = insertBoardCounts p (boardCounts b)
  }

removeBoard :: Address -> Piece -> Board -> Board
removeBoard a p@(Piece Man First) b = b {
    bFirstMen = LS.delete (aLabel a) (bFirstMen b),
    bOccupied = LS.delete (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece Man Second) b = b {
    bSecondMen = LS.delete (aLabel a) (bSecondMen b),
    bOccupied = LS.delete (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece King First) b = b {
    bFirstKings = LS.delete (aLabel a) (bFirstKings b),
    bOccupied = LS.delete (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }
removeBoard a p@(Piece King Second) b = b {
    bSecondKings = LS.delete (aLabel a) (bSecondKings b),
    bOccupied = LS.delete (aLabel a) (bOccupied b)
--     boardCounts = removeBoardCounts p (boardCounts b)
  }

newTBoardMap :: Int -> IO (TBoardMap a)
newTBoardMap = HT.new

putBoardMap :: TBoardMap a -> Board -> a -> IO ()
putBoardMap bmap board value = HT.write bmap (boardHash board) value

putBoardMapWith :: TBoardMap a -> (a -> a -> a) -> Board -> a -> IO ()
putBoardMapWith bmap plus board value = HT.writeWith bmap plus (boardHash board) value

lookupBoardMap :: TBoardMap a -> Board -> IO (Maybe a)
lookupBoardMap bmap board = HT.read bmap (boardHash board)

resetBoardMap :: TBoardMap a -> IO ()
resetBoardMap bmap = HT.reset bmap

------------------

aIndex :: Address -> FieldIndex
aIndex a = fromIntegral (labelColumn l) * 16 + fromIntegral (labelRow l)
  where l = aLabel a

buildLabelMap :: Line -> Line -> [(Label, a)] -> LabelMap a
buildLabelMap nrows ncols pairs =
  IM.fromList [(labelIndex label, value) |  (label, value) <- pairs]

lookupLabel :: Label -> LabelMap a -> Maybe a
lookupLabel label lmap = IM.lookup (labelIndex label) lmap

emptyAddressMap :: BoardSize -> AddressMap a
emptyAddressMap (nrows,ncols) = IM.empty

lookupAddress :: Address -> AddressMap a -> Maybe a
lookupAddress a amap = IM.lookup (aIndex a) amap

setAddress :: Address -> a -> AddressMap a -> AddressMap a
setAddress a x amap = IM.insert (aIndex a) x amap

removeAddress :: Address -> AddressMap a -> AddressMap a
removeAddress a amap = IM.delete (aIndex a) amap

addressMapContains :: Label -> AddressMap a -> Bool
addressMapContains label amap = IM.member (labelIndex label) amap

findLabels :: (a -> Bool) -> AddressMap a -> [Label]
findLabels fn amap = [unpackIndex idx | idx <- IM.keys $ IM.filter fn amap]

countAddresses :: (a -> Bool) -> AddressMap a -> Int
countAddresses fn amap = length $ findLabels fn amap

occupiedLabels :: AddressMap a -> [(Label, a)]
occupiedLabels amap = [(unpackIndex idx, value) | (idx, value) <- IM.assocs amap]

labelMapKeys :: LabelMap a -> [Label]
labelMapKeys lmap = map unpackIndex $ IM.keys lmap

--------------------

instance Hashable IS.IntSet where
  hashWithSalt salt set = hashWithSalt salt (IS.toList set)

instance Show Board where
  show b = printf "{First Men: %s; Second Men: %s; First Kings: %s; Second Kings: %s}"
              (show $ LS.toList $ bFirstMen b)
              (show $ LS.toList $ bSecondMen b)
              (show $ LS.toList $ bFirstKings b)
              (show $ LS.toList $ bSecondKings b)

