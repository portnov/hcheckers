{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module AI.AlphaBeta.Types where

import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Metrics as Metrics
import qualified Control.Concurrent.ReadWriteLock as RWL
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.HashPSQ as PQ
import Data.Word
import Data.Binary
import Data.Store
import Data.Typeable
import Data.Default
import GHC.Generics
import System.Clock
import System.Posix.Types
import System.IO.RandomAccessFile
import System.Log.Heavy

import Core.Types
import Core.Parallel

data AlphaBeta rules eval = AlphaBeta AlphaBetaParams rules eval
  deriving (Eq, Ord, Show, Typeable)

data AlphaBetaParams = AlphaBetaParams {
    abDepth :: Int
  , abStartDepth :: Maybe Int
  , abCombinationDepth :: Int
  , abBaseTime :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance Default AlphaBetaParams where
  def = AlphaBetaParams {
          abDepth = 2
        , abStartDepth = Nothing
        , abCombinationDepth = 8
        , abBaseTime = Nothing
        }

-- Calculation depth parameters
data DepthParams = DepthParams {
    dpTarget :: Int     -- ^ Target depth: how deep we currently want to calculate the tree
  , dpCurrent :: Int    -- ^ Currently achieved depth
  , dpMax :: Int        -- ^ Maximum allowed depth
  , dpMin :: Int        -- ^ Minimum allowed depth
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Store DepthParams

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

type PerBoardData = M.Map DepthParams CacheItem

type AIData = BoardMap PerBoardData

type StorageKey = (DepthParams, BoardKey)

type StorageValue = CacheItemSide

type ScoreMoveInput rules eval = 
  (AlphaBeta rules eval, AICacheHandle rules eval, Side, DepthParams, Board, PossibleMove)

data AICache rules eval = AICache {
    aicDirty :: Bool
  , aicProcessor :: Processor Move (ScoreMoveInput rules eval) (Move, Score)
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

-- | Handle to the instance of AI storage
-- and related structures
data AICacheHandle rules eval = AICacheHandle {
    aichRules :: rules
  , aichData :: TVar (AICache rules eval)
  , aichWriteQueue :: WriteQueue
  , aichCleanupQueue :: CleanupQueue
  , aichCurrentCounts :: TVar BoardCounts
  , aichIndexFile :: Maybe FHandle
  , aichDataFile :: Maybe FHandle
  }

type WriteQueue = TChan (Board, DepthParams, Side, StorageValue)

type CleanupQueue = TVar (PQ.HashPSQ QueueKey TimeSpec ())

type FileDescriptor = MMaped

-- | File handle
data FHandle = FHandle {
    fhOffset :: FileOffset
  , fhHandle :: FileDescriptor
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

runStorage :: (GameRules rules, Evaluator eval) => AICacheHandle rules eval -> Storage a -> Checkers a
runStorage handle actions = do
  lts <- asks csLogging
  let indexHandle = aichIndexFile handle
  let dataHandle = aichDataFile handle
  let bsize = boardSize (aichRules handle)
  metrics <- Metrics.getMetrics
  let initState = StorageState lts metrics bsize indexHandle dataHandle
  liftIO $ evalStateT actions initState
  
