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
import Control.Monad.Catch (catch, SomeException)
import Control.Concurrent.STM
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Data.Maybe
import Data.Word
import Data.Text.Format.Heavy
import Data.Store
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Bytes.Put
import System.Clock
import Text.Printf
import GHC.Generics
import System.Posix.Types
import qualified System.IO.RandomAccessFile as File
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import qualified Core.Monitoring as Monitoring
import AI.AlphaBeta.Types

maxPieces :: Integer
maxPieces = 30

encodeBoard :: Board -> B.ByteString
encodeBoard board = runPutS $ runEncode $ encodeB board
  where

    encodePiece Nothing =
      putBit False
    encodePiece (Just (Piece Man First)) = do
      putBit True
      putBit False
      putBit False
    encodePiece (Just (Piece Man Second)) = do
      putBit True
      putBit False
      putBit True
    encodePiece (Just (Piece King First)) = do
      putBit True
      putBit True
      putBit False
    encodePiece (Just (Piece King Second)) = do
      putBit True
      putBit True
      putBit True

    encodeB b = do
      forM_ (allPieces b) encodePiece
      

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

putWriteQueue :: WriteQueue -> (Board, DepthParams, Side, StorageValue) -> STM ()
putWriteQueue = writeTChan

checkWriteQueue :: WriteQueue -> STM (Maybe (Board, DepthParams, Side, StorageValue))
checkWriteQueue = tryReadTChan

getFd :: FileType -> Storage FileDescriptor
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
    Just fh -> return fh

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
  result <- liftIO $ File.readBytes fd (fromIntegral currentOffset) (fromIntegral size)
  seek file $ fromIntegral size + currentOffset
  return result
  
writeBytes :: FileType -> B.ByteString -> Storage ()
writeBytes file bstr = do
  fd <- getFd file
  currentOffset <- tell file
  result <- liftIO $ File.writeBytes fd (fromIntegral currentOffset) bstr
  let size = B.length bstr
  seek file $ fromIntegral size + currentOffset

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

writeData :: forall a. Data.Store.Store a => FileType -> a -> Storage ()
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
    fail $ "readDataSized: zero data size, offset " ++ show offset
  liftIO $ Data.Store.decodeIO bstr

writeDataSized :: forall a. Data.Store.Store a => FileType -> a -> Storage ()
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
indexBlockSize (nrows, ncols) = 256 * indexRecordSize

dataBlockSize :: FileOffset
dataBlockSize = 128

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

lookupFileB :: B.ByteString -> Storage (Maybe PerBoardData)
lookupFileB bstr = do
    st <- get
    case ssData st of
      Nothing -> return Nothing
      Just _ -> loop 0 bstr
  where
    loop blockNumber bstr
      | B.length bstr == 1 = do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let dataBlockNumber = irDataBlock record
          if dataBlockNumber == unexistingBlock
            then return Nothing
            else do
                   let dataOffset = calcDataBlockOffset dataBlockNumber
                   seek DataFile dataOffset
                   value <- readDataSized DataFile
                   return $ Just value
      | otherwise = do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let nextBlockNumber = irIndexBlock record
          if nextBlockNumber == unexistingBlock
            then return Nothing
            else loop nextBlockNumber (B.tail bstr)
          

-- | Returns: (cached result, stats
lookupFile :: Board -> DepthParams -> Side -> Storage (Maybe Score, Maybe Stats)
lookupFile board depth side = Monitoring.timed "cache.lookup.file" $ do
  mbRecord <- lookupFileB (encodeBoard board)
  case mbRecord of
    Nothing -> return (Nothing, Nothing)
    Just record -> do
      let cached =
            case M.lookup (dpLast depth) (boardScores record) of
               Nothing -> Nothing
               Just ci -> case side of
                            First -> cisScore `fmap` ciFirst ci
                            Second -> cisScore `fmap` ciSecond ci
          stats = boardStats record
      return (cached, stats)

lookupStatsFile :: Board -> Storage (Maybe Stats)
lookupStatsFile board = Monitoring.timed "stats.lookup.file" $ do
  mbItem <- lookupFileB (encodeBoard board)
  return $ join $ boardStats `fmap` mbItem

putRecordFileB :: B.ByteString -> PerBoardData -> Storage ()
putRecordFileB bstr newData = do
    st <- get
    case ssData st of
      Nothing -> return ()
      Just _ -> tryBlock 0 bstr
  where
    tryBlock blockNumber bstr
      | B.length bstr == 1 = do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let dataBlockNumber = irDataBlock record
          if dataBlockNumber == unexistingBlock 
            then do
                 Monitoring.increment "storage.data.block.created"
                 newDataBlock <- createDataBlock
                 let record' = record {irDataBlock = newDataBlock}
                 seek IndexFile idxOffset
                 writeData IndexFile record'
                 seek DataFile $ calcDataBlockOffset newDataBlock
                 writeDataSized DataFile newData
                 return ()
            else do
                 Monitoring.increment "storage.data.block.reused"
                 let dataOffset = calcDataBlockOffset dataBlockNumber
                 seek DataFile dataOffset
                 oldData <- readDataSized DataFile
                              `catch`
                                (\(e :: SomeException) -> do
                                    $reportError "putRecordFileB: {}" (Single $ show e)
                                    return mempty
                                )
                 let newData' = oldData <> newData
                 seek DataFile dataOffset
                 writeDataSized DataFile newData'
                 return ()
      | otherwise = do
          bsize <- gets ssBoardSize
          let idxOffset = calcIndexOffset bsize blockNumber (B.head bstr)
          seek IndexFile idxOffset
          record <- readData IndexFile
          let nextBlockNumber = irIndexBlock record
          if nextBlockNumber == unexistingBlock
            then do
                 Monitoring.increment "storage.index.block.created"
                 newIndexBlock <- createIndexBlock
                 let record' = record {irIndexBlock = newIndexBlock}
                 seek IndexFile idxOffset
                 writeData IndexFile record'
                 tryBlock newIndexBlock (B.tail bstr)
            else do
                 Monitoring.increment "storage.index.block.reused"
                 tryBlock nextBlockNumber (B.tail bstr)
    
    createIndexBlock = do
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

    createDataBlock = do
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

putRecordFile :: Board -> DepthParams -> Side -> StorageValue -> Storage ()
putRecordFile board depth side value = Monitoring.timed "cache.put.file" $ do
  let bstr = encodeBoard board
      newData = PerBoardData (M.singleton (dpLast depth) item) Nothing
      item = case side of
               First -> CacheItem {ciFirst = Just value, ciSecond = Nothing}
               Second -> CacheItem {ciFirst = Nothing, ciSecond = Just value}
  putRecordFileB bstr newData

putStatsFile :: Board -> Stats -> Storage ()
putStatsFile board stats = do
  let newData = PerBoardData M.empty (Just stats)
      bstr = encodeBoard board
  putRecordFileB bstr newData

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

readDataIO :: forall a h. (Data.Store.Store a, File.FileAccess h) => h -> File.Offset -> IO a
readDataIO file offset = do
  bstr <- File.readBytes file offset (fromIntegral $ sizeOf (error "unknown data size to read!" :: a))
  when (B.null bstr) $ do
    fail $ "readDataIO: unexpected EOF, offset " ++ show offset
  Data.Store.decodeIO bstr

readDataSizedIO :: forall a h. (Data.Store.Store a, File.FileAccess h) => h -> File.Offset -> IO a
readDataSizedIO file offset = do
  size <- readDataIO file offset :: IO Word16
  bstr <- File.readBytes file (offset + 2) (fromIntegral size)
  when (B.null bstr) $ do
    fail $ printf "readDataSizedIO: zero data size, offset %s, size %s" (show offset) (show size)
  Data.Store.decodeIO bstr

dumpIndexBlock :: File.FileAccess h => h -> BoardSize -> IndexBlockNumber -> IO ()
dumpIndexBlock h bsize n = do
  forM_ [0 .. 255] $ \char -> do
    let offset = fromIntegral $ calcIndexOffset bsize n char
    record <- readDataIO h offset
    when (irDataBlock record /= unexistingBlock || irIndexBlock record /= unexistingBlock) $
      printf "Char #%d: next index #%d, data block #%d\n" char (irIndexBlock record) (irDataBlock record)

checkDataFile :: FilePath -> IO ()
checkDataFile path = do
  let params = File.MMapedParams (1024*1024) False
  file <- File.initFile params path
  nBlocks <- readDataIO file 0 :: IO DataBlockNumber
  forM_ [0 .. nBlocks - 1] $ \i -> do
      let start = fromIntegral $ calcDataBlockOffset i
      size <- readDataIO file start :: IO Word16
      when (size > 0) $ do
        bstr <- File.readBytes file (start + 2) (fromIntegral size)
        record <- Data.Store.decodeIO bstr :: IO PerBoardData
        printf "Block #%d: data: %s\n" i (show record)

checkDataFile' :: FilePath -> IO ()
checkDataFile' path = do
  let params = File.MMapedParams (1024*1024) False
  file <- File.initFile params path
  nBlocks <- readDataIO file 0 :: IO DataBlockNumber
  forM_ [0 .. nBlocks - 1] $ \i -> do
      let start = fromIntegral $ calcDataBlockOffset i
      size <- readDataIO file start :: IO Word16
      when (size > 0) $ do
        bstr <- File.readBytes file (fromIntegral $ start + 2) (fromIntegral size)
        record <- Data.Store.decodeIO bstr :: IO PerBoardData
        case boardStats record of
          Nothing -> return ()
          Just stats -> 
            when (statsCount stats > 10) $
              printf "Block #%d: data: %s\n" i (show record)

data ParserState = ParserState {
    psIndex :: File.MMaped
  , psUnfinished :: M.Map IndexBlockNumber BL.ByteString
  , psFinished :: M.Map BL.ByteString DataBlockNumber
  }

loadIndex :: StateT ParserState IO ()
loadIndex = do
    index <- gets psIndex
    header <- liftIO $ readDataIO index 0
    let n = ihBlocksCount header
    forM_ [0 .. n-1] loadBlock
  where
    bsize = (8,8)

    loadBlock :: IndexBlockNumber -> StateT ParserState IO ()
    loadBlock i = do
      index <- gets psIndex
      finished <- gets psFinished
      liftIO $ printf "Block #%d; finished: %d\n" i (M.size finished)
      records <- forM [0 .. 255] $ \char -> do
        let offset = calcIndexOffset bsize i char
        liftIO $ readDataIO index (fromIntegral offset)
      unfinished <- gets psUnfinished
      modify $ \st -> st {psUnfinished = M.delete i (psUnfinished st)}
      let prefix = fromMaybe "" $ M.lookup i unfinished
      forM_ (zip [0..] records) $ \(j, record) -> do
        let prefix' = prefix `BL.append` BL.singleton j
        when (irDataBlock record /= unexistingBlock) $
          modify $ \st -> st {psFinished = M.insert prefix' (irDataBlock record) (psFinished st)}
        when (irIndexBlock record /= unexistingBlock) $
          modify $ \st -> st {psUnfinished = M.insert (irIndexBlock record) prefix' (psUnfinished st)}
  
loadIndexIO :: FilePath -> IO (M.Map BL.ByteString DataBlockNumber)
loadIndexIO indexPath = do 
  let params = File.MMapedParams (1024*1024) False
  index <- File.initFile params indexPath
  let st = ParserState index M.empty M.empty
  st' <- execStateT loadIndex st
  File.closeFile index
  putStrLn "index loaded."
  return $ psFinished st'

-- loadDataIO :: FilePath -> FilePath -> IO [(Board, PerBoardData)]
-- loadDataIO indexPath dataPath = do
--   index <- loadIndexIO indexPath
--   let params = File.MMapedParams (1024*1024) False
--   file <- File.initFile params
--   forM (M.assocs index) $ \(bstr, block) -> do
    

