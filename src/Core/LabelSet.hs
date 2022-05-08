{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ViewPatterns #-}
module Core.LabelSet where

import Data.Word
import Data.Bits
import GHC.Generics
import Data.Store
import Data.Binary
import Data.Hashable

letters :: [Char]
letters = ['a' .. 'z']

-- | Number of row / column of the board
type Line = Word8

type BoardSize = (Line, Line)

-- | Label is a coordinate of field on the board.
newtype Label = Label Word8
  deriving (Eq, Ord, Generic)

instance Show Label where
  show l = letter : show (labelRow l + 1)
    where
      letter = letters !! fromIntegral (labelColumn l)

instance Binary Label

instance Store Label where
  size = ConstSize 1

  poke (Label x) = poke x
  peek = Label <$> peek

instance Hashable Label where
  hashWithSalt salt (Label x) = hashWithSalt salt x

mkLabel :: Line -> Line -> Label
mkLabel col row = Label (col * 16 + row)

labelRow :: Label -> Line
labelRow (Label n) = n `mod` 16

labelColumn :: Label -> Line
labelColumn (Label n) = n `div` 16

labelTuple :: Label -> (Line, Line)
labelTuple (Label n) = (n `div` 16, n `mod` 16)

flipLabel :: BoardSize -> Label -> Label
flipLabel (nrows, ncols) (labelTuple -> (col, row)) = mkLabel (ncols - col - 1) (nrows - row - 1)

type FieldIndex = Int

data LabelSet =
    LabelSet192 !Word64 !Word64 !Word64
  deriving (Eq, Show)

mkIndex :: Line -> Line -> FieldIndex
mkIndex col row =  fromIntegral col * 16 + fromIntegral row

labelIndex :: Label -> FieldIndex
labelIndex (Label n) = fromIntegral n

unpackIndex :: FieldIndex -> Label
unpackIndex n = Label (fromIntegral n)

empty :: LabelSet
empty = LabelSet192 0 0 0

isEmpty :: LabelSet -> Bool
isEmpty (LabelSet192 0 0 0) = True
isEmpty _ = False

size :: LabelSet -> Int
size (LabelSet192 x y z) = popCount x + popCount y + popCount z

bitsList :: Word64 -> [FieldIndex]
bitsList w = [fromIntegral i | i <- [0..63], testBit w i]

toList :: LabelSet -> [Label]
toList (LabelSet192 x y z) =
    map unpackIndex $ bitsList x ++ map (+64) (bitsList y) ++ map (+128) (bitsList z)

uncons :: LabelSet -> Maybe (Label, LabelSet)
uncons set =
  case set of
    (LabelSet192 0 0 0) -> Nothing
    (LabelSet192 0 0 z) ->
      let i = check z
      in  Just (unpackIndex (128 + i), LabelSet192 0 0 (clearBit z i))
    (LabelSet192 0 y z) ->
      let i = check y
      in  Just (unpackIndex (64 + i), LabelSet192 0 (clearBit y i) z)
    (LabelSet192 x y z) ->
      let i = check x
      in  Just (unpackIndex i, LabelSet192 (clearBit x i) y z)
  where
    check x = go 0 x
    go i x
      | testBit x i = i
      | otherwise = go (i+1) x

singleton :: Label -> LabelSet
singleton  label =
    if idx < 64
      then LabelSet192 (bit idx) 0 0
      else if idx < 128
        then LabelSet192 0 (bit (idx - 64)) 0
        else LabelSet192 0 0 $ bit (idx - 128)
  where
    idx = fromIntegral $ labelIndex label

union :: LabelSet -> LabelSet -> LabelSet
union (LabelSet192 x1 y1 z1) (LabelSet192 x2 y2 z2) = LabelSet192 (x1 .|. x2) (y1 .|. y2) (z1 .|. z2)

unions :: [LabelSet] -> LabelSet
unions sets = foldr union empty sets

intersect :: LabelSet -> LabelSet -> LabelSet
intersect (LabelSet192 x1 y1 z1) (LabelSet192 x2 y2 z2) = LabelSet192 (x1 .&. x2) (y1 .&. y2) (z1 .&. z2)

difference :: LabelSet -> LabelSet -> LabelSet
difference (LabelSet192 x1 y1 z1) (LabelSet192 x2 y2 z2) = LabelSet192 (x1 .\. x2) (y1 .\. y2) (z1 .\. z2)
  where
    x .\. y = x .&. (complement y)

fromList :: [Label] -> LabelSet
fromList labels = foldr union empty $ map singleton labels

fromPredicate :: (Label -> Bool) -> LabelSet
fromPredicate p = fromList $ filter p $ map unpackIndex [0 .. 191]

insert :: Label -> LabelSet -> LabelSet
insert label (LabelSet192 x y z) =
  let idx = labelIndex label
  in  if idx < 64
        then LabelSet192 (setBit x idx) y z
        else if idx < 128
               then LabelSet192 x (setBit y (idx - 64)) z
               else LabelSet192 x y (setBit z (idx - 128))

delete :: Label -> LabelSet -> LabelSet
delete label (LabelSet192 x y z) =
  let idx = labelIndex label
  in  if idx < 64
        then LabelSet192 (clearBit x idx) y z
        else if idx < 128
               then LabelSet192 x (clearBit y (idx - 64)) z
               else LabelSet192 x y (clearBit z (idx - 128))

member :: Label -> LabelSet -> Bool
member label (LabelSet192 x y z) =
  let idx = labelIndex label
  in  if idx < 64
        then testBit x idx
        else if idx < 128
               then testBit y (idx - 64)
               else testBit z (idx - 128)

