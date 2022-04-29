{-# LANGUAGE ScopedTypeVariables #-}

module Core.HTable (
    HTable,
    new, read, toList,
    write, writeWith,
    reset
  ) where

import Control.Monad
import Prelude hiding (read)
import Control.Exception (bracket_)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector as V
-- import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Control.Concurrent.Lock as Lock
import Data.Word
import Foreign.Storable
import Foreign.Ptr

type Key = Int

data HTableValue a = HTableValue !Key !(Maybe a)

instance Storable a => Storable (HTableValue a) where
  sizeOf _ = sizeOf (undefined :: Key) + 1 + sizeOf (undefined :: a)
  alignment _ = alignment (undefined :: Key) + alignment (undefined :: a)

  peek ptr = do
    key <- peek (castPtr ptr) :: IO Key
    flag <- peekByteOff (castPtr ptr) (sizeOf key) :: IO Word8
    if flag == 0
      then return $ HTableValue key Nothing
      else do
        value <- peekByteOff (castPtr ptr) (sizeOf key + sizeOf flag)
        return $ HTableValue key (Just value)

  poke ptr (HTableValue key x) = do
    poke (castPtr ptr) key
    case x of
      Nothing -> pokeByteOff (castPtr ptr) (sizeOf key) (0 :: Word8)
      Just value -> do
        pokeByteOff (castPtr ptr) (sizeOf key) (1 :: Word8)
        pokeByteOff (castPtr ptr) (sizeOf key + sizeOf (1 :: Word8)) value

data HTable a = HTable {
    htLocks :: V.Vector Lock.Lock
  , htData :: MV.IOVector (HTableValue a)
  }

htable_size :: Int
htable_size = 2^26

locks_count :: Int
locks_count = 2^8

getLockIdx :: Key -> Int
getLockIdx k = k `mod` locks_count

new :: IO (HTable a)
new = do
  locks <- replicateM locks_count Lock.new
  items <- MV.replicate htable_size (HTableValue 0 Nothing)
  return $ HTable (V.fromList locks) items

read :: HTable a -> Key -> IO (Maybe a)
read ht key = do
  let key' = key `mod` htable_size
      lockIdx = getLockIdx key'
  Lock.with (htLocks ht V.! lockIdx) $ do
    HTableValue actualKey item <- MV.read (htData ht) key'
    if actualKey == key
      then return item
      else return Nothing

write :: HTable a -> Key -> a -> IO ()
write ht key value = do
  let key' = key `mod` htable_size
      lockIdx = getLockIdx key'
  Lock.with (htLocks ht V.! lockIdx) $ do
    MV.write (htData ht) key' (HTableValue key (Just value))

writeWith :: HTable a -> (a -> a -> a) -> Key -> a -> IO ()
writeWith ht op key value = do
  let key' = key `mod` htable_size
      lockIdx = getLockIdx key'
  Lock.with (htLocks ht V.! lockIdx) $ do
    HTableValue oldKey mbItem <- MV.read (htData ht) key'
    case mbItem of
      Just item -> MV.write (htData ht) key' $ HTableValue key (Just (op item value))
      Nothing -> MV.write (htData ht) key' $ HTableValue key (Just value)

reset :: HTable a -> IO ()
reset ht = bracket_ acquireAll releaseAll resetAll
  where
    acquireAll = V.forM_ (htLocks ht) Lock.acquire
    releaseAll = V.forM_ (V.reverse $ htLocks ht) Lock.release
    resetAll = MV.set (htData ht) (HTableValue 0 Nothing)

toList :: HTable a -> IO [(Key, a)]
toList ht = bracket_ acquireAll releaseAll readAll
  where
    acquireAll = V.forM_ (htLocks ht) Lock.acquire
    releaseAll = V.forM_ (V.reverse $ htLocks ht) Lock.release
    readAll = do
      allItems <- forM [0 .. htable_size-1] $ \i -> MV.read (htData ht) i
      let setItems = [(key, value) | HTableValue key (Just value) <- allItems]
      return setItems

