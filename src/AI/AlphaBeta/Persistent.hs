{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}

module AI.AlphaBeta.Persistent where

import Control.Monad
import Control.Monad.State
import Control.Monad.Catch (bracket_)
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import Data.Word
-- import Data.Binary
import Data.Store
import System.Clock
import System.IO 
import Text.Printf
import System.Posix.Types
import System.Posix.IO
import "unix-bytestring" System.Posix.IO.ByteString

import Core.Types
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
 - Each block starts with:
 - * a number of previous block in the chain. maxBound here means that this block is
 -   the first in the chain (there is no previous one).
 - * a number of data records in this block
 - After such block header, there go records themselve, in a format defined by
 - Data.Store.Store instance. There may be some free space (probably filled with
 - garbage) left between the end of last record in the block and the end of the block.
 -}

type BlockNumber = Word16

type Hash = Word16

boardHash :: Board -> Hash
boardHash (boardKey -> bk) = listHash (bkFirstMen bk) + listHash (bkSecondMen bk) +
                             listHash (bkFirstKings bk) + listHash (bkSecondKings bk)
  where
    listHash labels = sum $ map labelHash labels
    labelHash (Label col row) = 255 * fromIntegral col + fromIntegral row

maxPieces :: Integer
maxPieces = 30

boardCountsIdx :: BoardCounts -> Integer
boardCountsIdx (BoardCounts a b c d) =
  maxPieces * fromIntegral (a+c) +
  fromIntegral (b+d)

hashIndiciesCount :: Integer
hashIndiciesCount = 1024

sizeOf :: Data.Store.Store a => a -> ByteCount
sizeOf a =
  case Data.Store.size of
    Data.Store.VarSize fn -> fromIntegral $ fn a
    Data.Store.ConstSize n -> fromIntegral n

hashesCount :: Integer
hashesCount = fromIntegral (maxBound :: Hash) + 1

hashIndexSize :: ByteCount
hashIndexSize = fromIntegral hashesCount * blockNumberSize

allIndiciesSize :: ByteCount
allIndiciesSize = blockNumberSize + fromIntegral hashIndiciesCount * hashIndexSize

blockNumberSize :: ByteCount
blockNumberSize = sizeOf (0 :: BlockNumber)

blockHeaderSize :: ByteCount
blockHeaderSize = blockNumberSize + sizeOf (0 :: Word16) + sizeOf (0 :: Word16)

blockSize :: ByteCount
blockSize = 8192

unexistingBlock :: BlockNumber
unexistingBlock = maxBound

putCleanupQueue :: CleanupQueue -> QueueKey -> TimeSpec -> STM ()
putCleanupQueue var key now = modifyTVar var $ \queue ->
    let cleanupDelay = 60 * 1000 * 1000 * 1000
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

putWriteQueue :: WriteQueue -> (Board, Int, Side, StorageValue) -> STM ()
putWriteQueue = writeTChan

checkWriteQueue :: WriteQueue -> STM (Maybe (Board, Int, Side, StorageValue))
checkWriteQueue = tryReadTChan

underLock :: Board -> Storage rules a -> Storage rules a
underLock board action = do
  handle <- gets ssHandle
  mbLocked <- liftIO $ atomically $ readTVar (aichLockedBoard handle)
  case mbLocked of
    Nothing -> action
    Just locked -> if locked == boardKey board
                     then bracket_
                            (liftIO $ waitQSem (aichBoardLock handle))
                            (liftIO $ signalQSem (aichBoardLock handle))
                            action
                     else action

getFd :: Storage rules Fd
getFd = do
  aich <- gets ssHandle
  case aichFileHandle aich of
    Nothing -> fail "getFd: file is not open"
    Just fd -> return fd

readBytes :: ByteCount -> Storage rules B.ByteString
readBytes size = do
  fd <- getFd
  currentOffset <- gets ssFileOffset
  result <- liftIO $ fdPread fd (fromIntegral size) currentOffset
  modify $ \ss -> ss {ssFileOffset = fromIntegral size + ssFileOffset ss}
  return result
  
writeBytes :: B.ByteString -> Storage rules ByteCount
writeBytes bstr = do
  fd <- getFd
  currentOffset <- gets ssFileOffset
  result <- liftIO $ fdPwrite fd bstr currentOffset
  let size = B.length bstr
  modify $ \ss -> ss {ssFileOffset = fromIntegral size + ssFileOffset ss}
  return $ fromIntegral result

seek :: FileOffset -> Storage rules ()
seek offset = modify $ \ss -> ss {ssFileOffset = offset}

tell :: Storage rules FileOffset
tell = gets ssFileOffset

flush :: Storage rules ()
flush = return ()
--   fd <- getFd
--   handle <- liftIO $ fdToHandle fd
--   liftIO $ hFlush handle

isEof :: Storage rules Bool
isEof = return False
--   fd <- getFd
--   handle <- liftIO $ fdToHandle fd
--   liftIO $ hIsEOF handle

-- | Read an item of appropriate type from file handle.
-- Warning: this cannot work with data types which size is variable.
readData :: forall a rules. Data.Store.Store a => Storage rules a
readData = do
  bstr <- readBytes (fromIntegral $ sizeOf (error "unknown data size to read!" :: a))
  when (B.null bstr) $ do
    offset <- tell
    fail $ "readData: unexpected EOF, offset " ++ show offset
  liftIO $ Data.Store.decodeIO bstr

writeData :: forall a rules. Data.Store.Store a => a -> Storage rules ByteCount
writeData a = do
  let bstr = Data.Store.encode a
  writeBytes bstr

-- | Read an item of appropriate type from file handle.
-- Assumes there is data size in Word16 format before data itself.
readDataSized :: forall a rules. Data.Store.Store a => Storage rules a
readDataSized = do
  size <- readData :: Storage rules Word16
  when (fromIntegral size > blockSize) $ do
    offset <- tell
    fail $ printf "readDataSized: too large data (%d) at offset %s" size (show offset)
  bstr <- readBytes (fromIntegral size)
  when (B.null bstr) $ do
    offset <- tell
    fail $ "readDataSized: unexpected EOF, offset " ++ show offset
  liftIO $ Data.Store.decodeIO bstr

writeDataSized :: forall a rules. Data.Store.Store a => a -> Storage rules ByteCount
writeDataSized a = do
  let bstr = Data.Store.encode a
  let size = (fromIntegral $ B.length bstr) :: Word16
  writeData size
  writeBytes bstr

calcIdxOffset :: Board -> FileOffset
calcIdxOffset board = fromIntegral $
    fromIntegral blockNumberSize +
    fromIntegral hashIndexSize * fromIntegral (boardCountsIdx (boardCounts board)) +
      fromIntegral (boardHash board) * fromIntegral blockNumberSize

calcBlockOffset :: BlockNumber -> FileOffset
calcBlockOffset blockNumber = fromIntegral allIndiciesSize + fromIntegral blockNumber * fromIntegral blockSize

lookupFile :: forall rules. Board -> Int -> Side -> Storage rules (Maybe StorageValue)
lookupFile board depth side = do
  handle <- gets ssHandle
  case aichFileHandle handle of
    Nothing -> return Nothing
    Just file -> underLock board $ do
          let indexOffset = calcIdxOffset board
          seek indexOffset
          blockNumber <- readData
          lookupBlocks blockNumber (boardKey board)
  where
    lookupBlocks :: BlockNumber -> BoardKey -> Storage rules (Maybe StorageValue)
    lookupBlocks blockNumber key
      | blockNumber == unexistingBlock = return Nothing
      | otherwise = do
          let blockOffset = calcBlockOffset blockNumber
          seek blockOffset
          eof <- isEof
          if eof
            then do
                 -- printf "Block #%d, offset %d: EOF\n" blockNumber blockOffset
                 return Nothing
            else do
              prevBlockNumber <- readData 
              recordsCount <- readData :: Storage rules Word16
              freeSpaceOffset <- readData :: Storage rules Word16
--               liftIO $ printf "Reading %d records in block %d\n" recordsCount blockNumber
              records <- replicateM (fromIntegral recordsCount) $
                            readDataSized 
              let result = search key records
              case result of
                Just _ -> return result
                Nothing -> lookupBlocks prevBlockNumber key
    
    search :: BoardKey -> [(StorageKey, StorageValue)] -> Maybe StorageValue
    search key [] = Nothing
    search key (((bk,d,s), val) : rest)
      | bk == key && d == depth && s == side = Just val
      | otherwise = search key rest

putRecordFile :: Board -> Int -> Side -> StorageValue -> Storage rules ()
putRecordFile board depth side value = do
    handle <- gets ssHandle
    case aichFileHandle handle of
      Nothing -> liftIO $ putStrLn "file is not open"
      Just file -> underLock board $ do
          -- putStrLn $ "Put: " ++ show (boardKey board)
          let indexOffset = calcIdxOffset board
          position indexOffset
          blockNumber <- readData 
          let newData = Data.Store.encode newRecord
          if blockNumber == unexistingBlock
            then do
                 blockOffset <- createNewBlock indexOffset
                 position blockOffset
                 writeData unexistingBlock
                 writeData (1 :: Word16) -- number of records
                 writeData $ (fromIntegral (B.length newData) :: Word16) + fromIntegral blockHeaderSize + fromIntegral (sizeOf (0 :: Word16))
                 writeDataSized newRecord
            else do
                 let blockOffset = calcBlockOffset blockNumber
                 -- we are not interested in the number of previous block,
                 -- so we just skip it
                 position (blockOffset + fromIntegral blockNumberSize)
                 recordsCount <- readData :: Storage rules Word16
                 freeSpaceOffset <- readData :: Storage rules Word16
                 let newBlockSize = freeSpaceOffset +
                                    fromIntegral (sizeOf (0 :: Word16)) + -- for length of new data
                                    fromIntegral (B.length newData)
                 if newBlockSize > fromIntegral blockSize
                   then do
                     -- new record does not fit in existing block,
                     -- we have to create a new block
                     newBlockOffset <- createNewBlock indexOffset
                     position newBlockOffset
                     writeData blockNumber
                     writeData (1 :: Word16) -- number of records
                     writeData $ (fromIntegral (B.length newData) :: Word16) + fromIntegral blockHeaderSize + fromIntegral (sizeOf (0 :: Word16))
                     writeDataSized newRecord
                  else do
                    -- new record fits in existing block.
                    -- we are currently at the position after existing records
                    -- in the block, so we need to write a new record
                    -- and update the number of records in the block.
                    position $ blockOffset + fromIntegral freeSpaceOffset
                    writeDataSized newRecord
                    newFree <- tell
                    position (blockOffset + fromIntegral blockNumberSize)
                    writeData $ recordsCount+1
                    writeData (fromIntegral (newFree - blockOffset) :: Word16)
          flush
  where
    
    newRecord :: (StorageKey, StorageValue)
    newRecord = ((boardKey board, depth, side), value)

    position :: FileOffset -> Storage rules ()
    position offset = do
      seek offset
--       liftIO $ printf "position: %s\n" (show offset)

    createNewBlock :: FileOffset -> Storage rules FileOffset
    createNewBlock indexOffset = do
      handle <- gets ssHandle
      bracket_
        (liftIO $ waitQSem (aichBlocksCountLock handle))
        (liftIO $ signalQSem (aichBlocksCountLock handle)) $ do
          prevPosition <- tell
          -- read previous total number of blocks
          position 0
          blocksCount <- readData :: Storage rules Word16
          -- write new number of blocks
          position 0
          writeData (blocksCount + 1)

          let newBlockNumber = blocksCount -- for example, if there was 0 blocks previously, the number of new block is 0.
              newBlockOffset = calcBlockOffset newBlockNumber

          liftIO $ printf "Created new block: #%d, offset %s\n" newBlockNumber (show newBlockOffset)
          -- update number of newly last block in the chain in the hash-index
          position indexOffset
          writeData newBlockNumber

          -- position to new block and fill it with zeros
          position newBlockOffset
          writeBytes $ B.pack $ replicate (fromIntegral blockSize) 0
          position prevPosition
          return newBlockOffset

initFile :: Storage rules ()
initFile = do
  seek 0
  writeData (0 :: Word16)
  let size = fromIntegral hashIndiciesCount * fromIntegral hashesCount * fromIntegral blockNumberSize
  let empty = B.replicate size 0xff
  writeBytes empty
  return ()

repeatTimed :: forall m. MonadIO m => Int -> m Bool -> m ()
repeatTimed seconds action = do
    start <- liftIO $ getTime Monotonic
    run 0 start
  where
    run :: Int -> TimeSpec -> m ()
    run i start = do
      continue <- action
      when continue $ do
        time2 <- liftIO $ getTime Monotonic
        let delta = time2 - start
        if sec delta >= fromIntegral seconds
          then liftIO $ printf "timeout exhaused, done %d records\n" i
          else run (i+1) start
  
dumpFile :: FilePath -> IO ()
dumpFile path = withFile path ReadMode $ \file -> do
      nBlocks <- readDataIO file :: IO Word16
      printf "Number of blocks: %d\n" nBlocks
      forM_ [0 .. hashIndiciesCount - 1] $ \hashIndexNr -> do
        printf "Hash index #%d:\n" hashIndexNr
        forM_ [0 .. hashesCount - 1] $ \hash -> do
          lastBlockNr <- readDataIO file :: IO BlockNumber
          when (lastBlockNr /= unexistingBlock) $ do
            printf "  Hash %d:\tlast block #%d\n" hash lastBlockNr
      forM_ [0 .. nBlocks - 1] $ \blockNr -> do
        let blockOffset = fromIntegral $ calcBlockOffset (fromIntegral blockNr)
        hSeek file AbsoluteSeek blockOffset
        printf "Block #%d, offset %d\n" blockNr blockOffset
        prevBlockNr <- readDataIO file :: IO BlockNumber
        printf "  Previous block: #%d\n" prevBlockNr
        nRecords <- readDataIO file :: IO Word16
        printf "  Number of records: %d\n" nRecords
        free <- readDataIO file :: IO Word16
        printf "  Free data offset: %d\n" free
        when (nRecords > 0) $ do
          forM_ [0 .. nRecords - 1] $ \recordNr -> do
            printf "    Record #%d:\n" recordNr
            ((bk,depth,side),item) <- readDataSizedIO file :: IO (StorageKey, StorageValue)
            printf "      Board key: %s\n" (show bk)
            printf "      Depth: %d, side: %s\n" depth (show side)
            printf "      Value: %s\n" (show item)

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
  when (fromIntegral size > blockSize) $ do
    offset <- hTell file
    fail $ printf "readDataSized: too large data (%d) at offset %s" size (show offset)
  bstr <- B.hGet file (fromIntegral size)
  when (B.null bstr) $ do
    offset <- hTell file
    fail $ printf "readDataSized: unexpected EOF, offset %s, size %s" (show offset) (show size)
  Data.Store.decodeIO bstr


