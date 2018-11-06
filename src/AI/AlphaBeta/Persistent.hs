{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.AlphaBeta.Persistent where

import Control.Monad
import Control.Monad.State
import Control.Monad.Catch (bracket_)
import qualified Control.Monad.Metrics as Metrics
import Control.Concurrent.STM
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import qualified Data.IntSet as IS
import Data.Word
import qualified Data.Binary
import qualified Data.Binary.Put
import Data.Store
import System.Clock
import System.IO 
import Text.Printf
import GHC.Generics
import System.Posix.Types
import "unix-bytestring" System.Posix.IO.ByteString
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.BoardMap
import AI.AlphaBeta.Types

{-
 - File structure description.
 -
 - First, goes one number: the total number of data blocks.
 - Then goes @hashIndiciesCount@ of hash-indicies.
 - The hash-index is an array of @hashesCount@ of BlockNumber numbers.
 -
 - The number of hash-index to which the board belongs is defined by converting
 - board's BoardCounts to number (see boardCountsIdx).
 -
 - The number of record in the hash-index to which the board belongs is defined
 - by hashing the BoardKey of that board.
 -
 - The BlockNumber in the hash-index is the number of *last* data block in the
 - chain of blocks, which contain data about similar boards. maxBound here
 - means that there are no blocks with such data.
 -
 - After hash-indicies, there go some (variable) number of data blocks.
 - Size of blocks is constant and hardcoded.
 - Each block starts with a block header:
 - * a number of previous block in the chain. maxBound here means that this block is
 -   the first in the chain (there is no previous one).
 - * a number of data records in this block
 - * offset of first free byte in the block w.r.t. block start
 - After such block header, there go records themselve, in a format defined by
 - Data.Store.Store instance. There may be some free space (probably filled with
 - garbage) left between the end of last record in the block and the end of the block.
 -}

maxPieces :: Integer
maxPieces = 30

encodeBoard :: BoardSize -> BoardKey -> B.ByteString
encodeBoard (nrows, ncols) board = BL.toStrict $ Data.Binary.Put.runPut (encodeBk board)
  where

    encodeLabel (Label col row) = do
      let half = ncols `div` 2
          row' = nrows - row - 1
          n = row' * half + (col `div` 2)
      Data.Binary.put (n :: Word8)

    encodeBk bk = do
      Data.Binary.put (fromIntegral (IS.size (bkFirstMen bk)) :: Word8)
      Data.Binary.put (fromIntegral (IS.size (bkSecondMen bk)) :: Word8)
      Data.Binary.put (fromIntegral (IS.size (bkFirstKings bk)) :: Word8)
      Data.Binary.put (fromIntegral (IS.size (bkSecondKings bk)) :: Word8)
      forM_ (labelSetToList $ bkFirstMen bk) encodeLabel
      forM_ (labelSetToList $ bkSecondMen bk) encodeLabel
      forM_ (labelSetToList $ bkFirstKings bk) encodeLabel
      forM_ (labelSetToList $ bkSecondKings bk) encodeLabel
      

sizeOf :: Data.Store.Store a => a -> ByteCount
sizeOf a =
  case Data.Store.size of
    Data.Store.VarSize fn -> fromIntegral $ fn a
    Data.Store.ConstSize n -> fromIntegral n

unexistingBlock :: IndexBlockNumber
unexistingBlock = maxBound

putCleanupQueue :: CleanupQueue -> QueueKey -> TimeSpec -> STM ()
putCleanupQueue var key now = modifyTVar var $ \queue ->
    let cleanupDelay = TimeSpec {sec=10, nsec=0}
    in  PQ.insert key (now + cleanupDelay) () queue

checkCleanupQueue :: CleanupQueue -> TimeSpec -> STM (Maybe QueueKey)
checkCleanupQueue var now = do
  queue <- readTVar var
  case PQ.minView queue of
    Nothing -> return Nothing
    Just (key, time, _, queue') ->
      if time < now
        then do
          writeTVar var queue'
          return $ Just key
        else return Nothing

putWriteQueue :: WriteQueue -> (BoardKey, Int, Side, StorageValue) -> STM ()
putWriteQueue = writeTChan

checkWriteQueue :: WriteQueue -> STM (Maybe (BoardKey, Int, Side, StorageValue))
checkWriteQueue = tryReadTChan

data AccessType = ReadAccess | WriteAccess

withLock :: RWL.RWLock -> AccessType -> Storage a -> Storage a
withLock lock ReadAccess action =
  bracket_
    (liftIO $ RWL.acquireRead lock)
    (liftIO $ RWL.releaseRead lock)
    action
withLock lock WriteAccess action =
  bracket_
    (liftIO $ RWL.acquireWrite lock)
    (liftIO $ RWL.releaseWrite lock)
    action

underBlockLock :: FileType -> AccessType -> IndexBlockNumber -> Storage a -> Storage a
underBlockLock file access n action = do
  fh <- getFh file
  let locksVar = lBlockLocks $ fhLocks fh
  newLock <- liftIO RWL.new
  lock <- liftIO $ atomically $ do
            locks <- readTVar locksVar
            case M.lookup n locks of
              Just lock -> return lock
              Nothing -> do
                writeTVar locksVar $ M.insert n newLock locks
                return newLock
  withLock lock access action

underGeneralLock :: FileType -> AccessType -> Storage a -> Storage a
underGeneralLock file access action = do
  fh <- getFh file
  let lock = lBlocksCount $ fhLocks fh
  withLock lock access action

getFd :: FileType -> Storage Fd
getFd file = do
  fh <- getFh file
  return $ fhHandle fh

getFh :: FileType -> Storage FHandle
getFh file = do
  let selector = case file of
                   IndexFile -> ssIndex
                   DataFile -> ssData
  st <- get
  case selector st of
    Nothing -> fail "getFh: file is not open"
    Just fh -> return $ fh

updateFh :: FileType -> (FHandle -> FHandle) -> Storage ()
updateFh IndexFile fn = do
  st <- get
  case ssIndex st of
    Nothing -> fail "updateFh: index file is not open"
    Just fh -> put $ st {ssIndex = Just $ fn fh}
updateFh DataFile fn = do
  st <- get
  case ssData st of
    Nothing -> fail "updateFh: data file is not open"
    Just fh -> put $ st {ssData = Just $ fn fh}

tell :: FileType -> Storage FileOffset
tell file = do
  fh <- getFh file
  return $ fhOffset fh

seek :: FileType -> FileOffset -> Storage ()
seek file offset = do
  updateFh file $ \fh -> fh {fhOffset = offset}

readBytes :: FileType -> ByteCount -> Storage B.ByteString
readBytes file size = do
  fd <- getFd file
  currentOffset <- tell file
  result <- liftIO $ fdPread fd (fromIntegral size) currentOffset
  seek file $ fromIntegral size + currentOffset
  return result
  
writeBytes :: FileType -> B.ByteString -> Storage ByteCount
writeBytes file bstr = do
  fd <- getFd file
  currentOffset <- tell file
  result <- liftIO $ fdPwrite fd bstr currentOffset
  let size = B.length bstr
  seek file $ fromIntegral size + currentOffset
  return $ fromIntegral result

flush :: Storage ()
flush = return ()
--   fd <- getFd
--   handle <- liftIO $ fdToHandle fd
--   liftIO $ hFlush handle

isEof :: Storage Bool
isEof = return False
--   fd <- getFd
--   handle <- liftIO $ fdToHandle fd
--   liftIO $ hIsEOF handle

-- | Read an item of appropriate type from file handle.
-- Warning: this cannot work with data types which size is variable.
readData :: forall a. Data.Store.Store a => FileType -> Storage a
readData file = do
  bstr <- readBytes file (fromIntegral $ sizeOf (error "unknown data size to read!" :: a))
  when (B.null bstr) $ do
    offset <- tell file
    fail $ "readData: unexpected EOF, offset " ++ show offset
  liftIO $ Data.Store.decodeIO bstr

writeData :: forall a. Data.Store.Store a => FileType -> a -> Storage ByteCount
writeData file a = do
  let bstr = Data.Store.encode a
  writeBytes file bstr

-- | Read an item of appropriate type from file handle.
-- Assumes there is data size in Word16 format before data itself.
readDataSized :: forall a. Data.Store.Store a => FileType -> Storage a
readDataSized file = do
  size <- readData file :: Storage Word16
  bstr <- readBytes file (fromIntegral size)
  when (B.null bstr) $ do
    offset <- tell file
    fail $ "readDataSized: unexpected EOF, offset " ++ show offset
  liftIO $ Data.Store.decodeIO bstr

writeDataSized :: forall a. Data.Store.Store a => FileType -> a -> Storage ByteCount
writeDataSized file a = do
  let bstr = Data.Store.encode a
  let size = (fromIntegral $ B.length bstr) :: Word16
  writeData file size
  writeBytes file bstr

dataHeaderSize :: FileOffset
dataHeaderSize = fromIntegral $ sizeOf (0 :: DataBlockNumber)

indexHeaderSize :: FileOffset
indexHeaderSize = fromIntegral $ sizeOf (0 :: IndexBlockNumber)

indexRecordSize :: FileOffset
indexRecordSize = fromIntegral (sizeOf (0 :: DataBlockNumber) + sizeOf (0 :: IndexBlockNumber))

indexBlockSize :: BoardSize -> FileOffset
indexBlockSize (nrows, ncols) = fromIntegral (nrows * ncols `div` 2) * indexRecordSize

dataBlockSize :: FileOffset
dataBlockSize = 2048

calcIndexBlockOffset :: BoardSize -> IndexBlockNumber -> FileOffset
calcIndexBlockOffset bsize n = indexHeaderSize + indexBlockSize bsize * fromIntegral n

calcDataBlockOffset :: DataBlockNumber -> FileOffset
calcDataBlockOffset n = dataHeaderSize + dataBlockSize * fromIntegral n

calcIndexOffset :: BoardSize -> IndexBlockNumber -> Word8 -> FileOffset
calcIndexOffset bsize block char =
  calcIndexBlockOffset bsize block + fromIntegral char * indexRecordSize

data IndexHeader = IndexHeader {
    ihBlocksCount :: IndexBlockNumber
  }
  deriving (Show, Generic)

instance Data.Store.Store IndexHeader where
  size = ConstSize $ fromIntegral $ sizeOf (0 :: IndexBlockNumber)

  poke h =
    poke $ ihBlocksCount h

  peek = do
    n <- peek
    return $ IndexHeader n

data DataHeader = DataHeader {
    dhBlocksCount :: DataBlockNumber
  }
  deriving (Show, Generic)

instance Data.Store.Store DataHeader where
  size = ConstSize $ fromIntegral $ sizeOf (0 :: DataBlockNumber)

  poke h =
    poke $ dhBlocksCount h

  peek = do
    n <- peek
    return $ DataHeader n

data IndexRecord = IndexRecord {
    irIndexBlock :: IndexBlockNumber
  , irDataBlock :: DataBlockNumber
  }
  deriving (Show)

instance Store IndexRecord where
  size = ConstSize $ fromIntegral $ sizeOf (0 :: IndexBlockNumber) + sizeOf (0 :: DataBlockNumber)

  poke r = do
    poke $ irIndexBlock r
    poke $ irDataBlock r

  peek = do
    idxBlock <- peek
    dataBlock <- peek
    return $ IndexRecord idxBlock dataBlock

lookupFileB :: B.ByteString -> Storage (Maybe (M.Map Int CacheItem))
lookupFileB bstr = do
    st <- get
    case ssData st of
      Nothing -> return Nothing
      Just _ -> loop 0 bstr
  where
    loop blockNumber bstr
      | B.length bstr == 1 = underBlockLock IndexFile ReadAccess blockNumber $ do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let dataBlockNumber = irDataBlock record
          if dataBlockNumber == unexistingBlock
            then return Nothing
            else underBlockLock DataFile ReadAccess dataBlockNumber $ do
                   let dataOffset = calcDataBlockOffset dataBlockNumber
                   seek DataFile dataOffset
                   value <- readDataSized DataFile
                   return $ Just value
      | otherwise = underBlockLock IndexFile ReadAccess blockNumber $ do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let nextBlockNumber = irIndexBlock record
          if nextBlockNumber == unexistingBlock
            then return Nothing
            else loop nextBlockNumber (B.tail bstr)
          

lookupFile :: BoardKey -> Int -> Side -> Storage (Maybe CacheItemSide)
lookupFile board depth side = Metrics.timed "cache.lookup.file" $ do
  bsize <- gets ssBoardSize
  mbItem <- lookupFileB (encodeBoard bsize board)
  case mbItem of
    Nothing -> return Nothing
    Just item -> case M.lookup depth item of
                   Nothing -> return Nothing
                   Just ci -> case side of
                                First -> return $ ciFirst ci
                                Second -> return $ ciSecond ci

putRecordFileB :: B.ByteString -> Int -> Side -> StorageValue -> Storage ()
putRecordFileB bstr depth side value = do
    st <- get
    case ssData st of
      Nothing -> return ()
      Just _ -> tryBlock 0 bstr
  where
    tryBlock blockNumber bstr
      | B.length bstr == 1 = underBlockLock IndexFile WriteAccess blockNumber $ do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let dataBlockNumber = irDataBlock record
          if dataBlockNumber == unexistingBlock 
            then do
                 newDataBlock <- createDataBlock
                 let record' = record {irDataBlock = newDataBlock}
                 seek IndexFile idxOffset
                 writeData IndexFile record'
                 seek DataFile $ calcDataBlockOffset newDataBlock
                 let newData = M.singleton depth newItem
                 writeDataSized DataFile newData
                 return ()
            else underBlockLock DataFile WriteAccess dataBlockNumber $ do
                   let dataOffset = calcDataBlockOffset dataBlockNumber
                   seek DataFile dataOffset
                   oldData <- readDataSized DataFile
                   let newData = updateData oldData
                   seek DataFile dataOffset
                   writeDataSized DataFile newData
                   return ()
      | otherwise = underBlockLock IndexFile WriteAccess blockNumber $ do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let nextBlockNumber = irIndexBlock record
          if nextBlockNumber == unexistingBlock
            then do
                 newIndexBlock <- createIndexBlock
                 let record' = record {irIndexBlock = newIndexBlock}
                 seek IndexFile idxOffset
                 writeData IndexFile record'
                 tryBlock newIndexBlock (B.tail bstr)
            else tryBlock nextBlockNumber (B.tail bstr)
    
    createIndexBlock = underGeneralLock IndexFile WriteAccess $ do
      seek IndexFile 0
      header <- readData IndexFile
      let newBlockNumber = ihBlocksCount header + 1
      let header' = header {ihBlocksCount = newBlockNumber}
      bsize <- gets ssBoardSize
      seek IndexFile 0
      writeData IndexFile header'
      seek IndexFile $ calcIndexBlockOffset bsize newBlockNumber
      let empty = B.replicate (fromIntegral $ indexBlockSize bsize) 0xff
      writeBytes IndexFile empty
      return newBlockNumber

    createDataBlock = underGeneralLock DataFile WriteAccess $ do
      seek DataFile 0
      header <- readData DataFile
      let newBlockNumber = dhBlocksCount header + 1
      let header' = header {dhBlocksCount = newBlockNumber}
      seek DataFile 0
      writeData DataFile header'
      seek DataFile $ calcDataBlockOffset newBlockNumber
      let empty = B.replicate (fromIntegral dataBlockSize) 0
      writeBytes DataFile empty
      return newBlockNumber

    updateData oldMap =
      M.insertWith updateItem depth newItem oldMap

    newItem = case side of
                First -> CacheItem {ciFirst = Just value, ciSecond = Nothing}
                Second -> CacheItem {ciFirst = Nothing, ciSecond = Just value}

    updateItem item1 item2 =
              case side of
                First -> item1 {ciFirst = ciFirst item2 `mplus` ciFirst item1}
                Second -> item2 {ciSecond = ciSecond item2 `mplus` ciSecond item1}

putRecordFile :: BoardKey -> Int -> Side -> StorageValue -> Storage ()
putRecordFile board depth side value = Metrics.timed "cache.put.file" $ do
  bsize <- gets ssBoardSize
  let bstr = encodeBoard bsize board
  putRecordFileB bstr depth side value

initFile :: Storage ()
initFile = do
  seek IndexFile 0
  writeData IndexFile $ IndexHeader 0
  bsize <- gets ssBoardSize
  let empty = B.replicate (fromIntegral $ indexBlockSize bsize) 0xff
  writeBytes IndexFile empty
  seek DataFile 0
  writeData DataFile $ DataHeader 0
  return ()

repeatTimed :: forall m. (MonadIO m, HasLogging m) => String -> Int -> m Bool -> m ()
repeatTimed label seconds action = do
    start <- liftIO $ getTime Monotonic
    run 0 start
  where
    run :: Int -> TimeSpec -> m ()
    run i start = do
      continue <- action
      if continue
        then do
            time2 <- liftIO $ getTime Monotonic
            let delta = time2 - start
            if sec delta >= fromIntegral seconds
              then $info "{}: timeout exhaused, done {} records" (label, i+1)
              else run (i+1) start
        else $info "{}: queue exhaused, done {} records" (label, i)
  
-- dumpFile :: FilePath -> IO ()
-- dumpFile path = withFile path ReadMode $ \file -> do
--       nBlocks <- readDataIO file :: IO Word16
--       printf "Number of blocks: %d\n" nBlocks
--       forM_ [0 .. hashIndiciesCount - 1] $ \hashIndexNr -> do
--         printf "Hash index #%d:\n" hashIndexNr
--         forM_ [0 .. hashesCount - 1] $ \hash -> do
--           lastBlockNr <- readDataIO file :: IO BlockNumber
--           when (lastBlockNr /= unexistingBlock) $ do
--             printf "  Hash %d, First:\tlast block #%d\n" hash lastBlockNr
--           lastBlockNr <- readDataIO file :: IO BlockNumber
--           when (lastBlockNr /= unexistingBlock) $ do
--             printf "  Hash %d, Second:\tlast block #%d\n" hash lastBlockNr
--       forM_ [0 .. nBlocks - 1] $ \blockNr -> do
--         let blockOffset = fromIntegral $ calcBlockOffset (fromIntegral blockNr)
--         hSeek file AbsoluteSeek blockOffset
--         printf "Block #%d, offset %d\n" blockNr blockOffset
--         header <- readDataIO file :: IO BlockHeader
--         let nRecords = bhRecordsCount header
--         printf "  Header: %s\n" (show header)
--         when (nRecords > 0) $ do
--           forM_ [0 .. nRecords - 1] $ \recordNr -> do
--             printf "    Record #%d:\n" recordNr
--             ((bk, depth),item) <- readDataSizedIO file :: IO (StorageKey, StorageValue)
--             printf "      Board key: %s\n" (show bk)
--             printf "      Depth: %d\n" depth
--             printf "      Value: %s\n" (show item)

readDataIO :: forall a. Data.Store.Store a => Handle -> IO a
readDataIO file = do
  bstr <- B.hGet file (fromIntegral $ sizeOf (error "unknown data size to read!" :: a))
  when (B.null bstr) $ do
    offset <- hTell file
    fail $ "readDataIO: unexpected EOF, offset " ++ show offset
  Data.Store.decodeIO bstr

readDataSizedIO :: forall a. Data.Store.Store a => Handle -> IO a
readDataSizedIO file = do
  size <- readDataIO file :: IO Word16
  bstr <- B.hGet file (fromIntegral size)
  when (B.null bstr) $ do
    offset <- hTell file
    fail $ printf "readDataSized: unexpected EOF, offset %s, size %s" (show offset) (show size)
  Data.Store.decodeIO bstr

dumpIndexBlock :: Handle -> BoardSize -> IndexBlockNumber -> IO ()
dumpIndexBlock h bsize n = do
  forM_ [0 .. 255] $ \char -> do
    hSeek h AbsoluteSeek $ fromIntegral $ calcIndexOffset bsize n char
    record <- readDataIO h
    when (irDataBlock record /= unexistingBlock || irIndexBlock record /= unexistingBlock) $
      printf "Char #%d: next index #%d, data block #%d\n" char (irIndexBlock record) (irDataBlock record)

checkDataFile :: FilePath -> IO ()
checkDataFile path = withFile path ReadMode $ \file -> do
  nBlocks <- readDataIO file :: IO DataBlockNumber
  forM_ [0 .. nBlocks - 1] $ \i -> do
    let start = fromIntegral $ calcDataBlockOffset i
    hSeek file AbsoluteSeek start
    size <- readDataIO file :: IO Word16
    printf "Block #%d: data size %d\n" i size

