{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module AI.AlphaBeta.Types where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Metrics as Metrics
import Control.Concurrent
import qualified Control.Concurrent.ReadWriteLock as RWL
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad.Catch (bracket_)
import qualified Data.Map as M
import qualified Data.HashPSQ as PQ
import qualified Data.ByteString as B
import Data.Word
import Data.Int
import Data.Binary
import Data.Store
import Data.Typeable
import Data.Default
import GHC.Generics
import System.FilePath
import System.Environment
import System.Directory
import System.Clock
-- import System.IO
import System.Posix.Types
import System.Posix.IO
import "unix-bytestring" System.Posix.IO.ByteString
import System.Log.Heavy

import Core.Types
import Core.BoardMap
import Core.Parallel

data AlphaBeta rules = AlphaBeta AlphaBetaParams rules
  deriving (Eq, Ord, Show, Typeable)

data AlphaBetaParams = AlphaBetaParams {
    abDepth :: Int
  , abStartDepth :: Maybe Int
  , abCombinationDepth :: Int
  , abThreads :: Int
  , abLoadCache :: Bool
  , abSaveCache :: Bool
  , abUseCacheMaxDepth :: Int
  , abUseCacheMaxPieces :: Int
  , abUseCacheMaxDepthPlus :: Int
  , abUseCacheMaxDepthMinus :: Int
  , abUpdateCacheMaxDepth :: Int
  , abUpdateCacheMaxPieces :: Int
  }
  deriving (Eq, Ord, Show)

instance Default AlphaBetaParams where
  def = AlphaBetaParams {
          abDepth = 2
        , abStartDepth = Nothing
        , abCombinationDepth = 8
        , abThreads = 4
        , abLoadCache = True
        , abSaveCache = False
        , abUseCacheMaxDepth = 8
        , abUseCacheMaxPieces = 24
        , abUseCacheMaxDepthPlus = 2
        , abUseCacheMaxDepthMinus = 0
        , abUpdateCacheMaxDepth = 6
        , abUpdateCacheMaxPieces = 8
        }

data DepthParams = DepthParams {
    dpTarget :: Int
  , dpCurrent :: Int
  , dpMax :: Int
  , dpMin :: Int
  }
  deriving (Eq, Show)

data CacheItemSide = CacheItemSide {
    cisScore :: ! Score
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary CacheItemSide

instance Store CacheItemSide

data CacheItem = CacheItem {
    ciFirst :: Maybe CacheItemSide
  , ciSecond :: Maybe CacheItemSide
  }
  deriving (Generic, Typeable, Show)

instance Binary CacheItem

instance Store CacheItem

type PerBoardData = M.Map Int CacheItem

type AIData = BoardMap PerBoardData

type StorageKey = (Int, BoardKey)

type StorageValue = CacheItemSide

type ScoreMoveInput rules =
  (AlphaBeta rules, AICacheHandle rules, Side, DepthParams, Board, Move)

data AICache rules = AICache {
    aicDirty :: Bool
  , aicProcessor :: Processor Move (ScoreMoveInput rules) (Move, Score)
  , aicData :: AIData
  }

type QueueKey = (BoardCounts, BoardKey)

type IndexBlockNumber = Word32
type DataBlockNumber = Word32

data FileType = IndexFile | DataFile
  deriving (Eq, Show)

data Locks = Locks {
    lBlocksCount :: RWL.RWLock
  , lBlockLocks :: TVar (M.Map IndexBlockNumber RWL.RWLock)
  }

data AICacheHandle rules = AICacheHandle {
    aichRules :: rules
  , aichData :: TVar (AICache rules)
  , aichWriteQueue :: WriteQueue
  , aichCleanupQueue :: CleanupQueue
  , aichIndexFile :: Maybe FHandle
  , aichDataFile :: Maybe FHandle
  }

type WriteQueue = TChan (BoardKey, Int, Side, StorageValue)

type CleanupQueue = TVar (PQ.HashPSQ QueueKey TimeSpec ())

data FHandle = FHandle {
    fhOffset :: FileOffset
  , fhHandle :: Fd
  , fhLocks :: Locks
  }

data StorageState = StorageState {
    ssLogging :: LoggingTState
  , ssMetrics :: Metrics.Metrics
  , ssBoardSize :: BoardSize
  , ssIndex :: Maybe FHandle
  , ssData :: Maybe FHandle
  }

type Storage a = StateT StorageState IO a

instance HasLogContext (StateT StorageState IO) where
  getLogContext = gets (ltsContext . ssLogging)

  withLogContext frame actions = do
    logging <- gets ssLogging
    let logging' = logging {ltsContext = frame : ltsContext logging} 
    modify $ \ss -> ss {ssLogging = logging'}
    result <- actions
    modify $ \ss -> ss {ssLogging = logging}
    return result
    
instance HasLogger (StateT StorageState IO) where
  getLogger = gets (ltsLogger . ssLogging)

  localLogger logger actions = do
    logging <- gets ssLogging
    let logging' = logging {ltsLogger = logger}
    modify $ \ss -> ss {ssLogging = logging'}
    result <- actions
    modify $ \ss -> ss {ssLogging = logging}
    return result

instance Metrics.MonadMetrics (StateT StorageState IO) where
  getMetrics = gets ssMetrics

runStorage :: GameRules rules => AICacheHandle rules -> Storage a -> Checkers a
runStorage handle actions = do
  lts <- asks csLogging
  let indexHandle = aichIndexFile handle
  let dataHandle = aichDataFile handle
  let bsize = boardSize (aichRules handle)
  metrics <- Metrics.getMetrics
  let initState = StorageState lts metrics bsize indexHandle dataHandle
  liftIO $ evalStateT actions initState
  
