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
import Core.BoardMap
import Core.Parallel

-- | Alpha-beta prunning AI engine.
-- It is parametrized by game rules, evaluator
-- and an instance of Evaluator.
data AlphaBeta rules eval = AlphaBeta AlphaBetaParams rules eval
  deriving (Eq, Ord, Show, Typeable)

data AlphaBetaParams = AlphaBetaParams {
    abDepth :: Int
  , abStartDepth :: Maybe Int
  , abCombinationDepth :: Int
  , abDeeperIfBad :: Bool
  , abMovesLowBound :: Int
  , abMovesHighBound :: Int
  , abBaseTime :: Maybe Int
  }
  deriving (Eq, Ord, Show)

instance Default AlphaBetaParams where
  def = AlphaBetaParams {
          abDepth = 2
        , abStartDepth = Nothing
        , abCombinationDepth = 8
        , abDeeperIfBad = False
        , abMovesLowBound = 4
        , abMovesHighBound = 8
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

dpLast :: DepthParams -> Int
dpLast dp = dpMax dp - dpCurrent dp

instance Store DepthParams
instance Binary DepthParams

data Stats = Stats {
    statsCount :: ScoreBase
  , statsMaxScore :: Score
  , statsMinScore :: Score
  , statsSumScore :: Score
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary Stats
instance Store Stats

instance Semigroup Stats where
  s1 <> s2 =
    Stats (statsCount s1 + statsCount s2)
          (max (statsMaxScore s1) (statsMaxScore s2))
          (min (statsMinScore s1) (statsMinScore s2))
          (statsSumScore s1 + statsSumScore s2)

instance Monoid Stats where
  mempty = Stats 0 0 0 0

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

instance Semigroup CacheItem where
  item1 <> item2 = CacheItem {
    ciFirst = ciFirst item2 `mplus` ciFirst item1,
    ciSecond = ciSecond item2 `mplus` ciSecond item1
  }

instance Monoid CacheItem where
  mempty = CacheItem Nothing Nothing

data PerBoardData = PerBoardData {
    boardScores :: M.Map Int CacheItem
  , boardStats :: Maybe Stats
  }
  deriving (Generic, Typeable, Show)

instance Semigroup PerBoardData where
  d1 <> d2 = PerBoardData {
    boardScores = M.unionWith (<>) (boardScores d1) (boardScores d2),
    boardStats = liftM2 (<>) (boardStats d1) (boardStats d2)
  }

instance Monoid PerBoardData where
  mempty = PerBoardData M.empty Nothing

instance Binary PerBoardData
instance Store PerBoardData

type AIData = BoardMap PerBoardData

type StorageKey = (DepthParams, BoardKey)

type StorageValue = CacheItemSide

-- | Input for the `scoreMove` method
data ScoreMoveInput rules eval = ScoreMoveInput {
    smiAi :: AlphaBeta rules eval
  , smiCache :: AICacheHandle rules eval
  , smiGameId :: GameId
  , smiDepth :: DepthParams
  , smiMove :: PossibleMove
  , smiNode :: Tree
  , smiAlpha :: Score
  , smiBeta :: Score
  }

data AICache rules eval = AICache {
    aicDirty :: Bool
  , aicProcessor :: Processor [MoveAction] (ScoreMoveInput rules eval) (PossibleMove, Tree, Score)
  , aicData :: AIData
  }

type QueueKey = (BoardCounts, BoardKey)

type IndexBlockNumber = Word32
type DataBlockNumber = Word32

data FileType = IndexFile | DataFile
  deriving (Eq, Show)

-- | Handle to the instance of AI storage
-- and related structures
data AICacheHandle rules eval = AICacheHandle {
    aichRules :: rules
  , aichData :: TVar (AICache rules eval)
  , aichCurrentNodes :: TVar (M.Map GameId Tree)
  , aichWriteQueue :: WriteQueue
  , aichCleanupQueue :: CleanupQueue
  , aichCurrentCounts :: TVar BoardCounts
  , aichIndexFile :: Maybe FHandle
  , aichDataFile :: Maybe FHandle
  }

data Tree = Node {
    nodeBoard :: Board
  , nodeSide :: Side
  , nodeChildren :: [(PossibleMove, Tree)]
  }
  deriving (Show)

type WriteQueue = TChan (Board, DepthParams, Side, StorageValue)

type CleanupQueue = TVar (PQ.HashPSQ QueueKey TimeSpec ())

type FileDescriptor = MMaped

-- | File handle
data FHandle = FHandle {
    fhOffset :: FileOffset
  , fhHandle :: FileDescriptor
  }

-- | State for the Storage monad
data StorageState = StorageState {
    ssLogging :: LoggingTState
  , ssMetrics :: Metrics.Metrics
  , ssMetricsEnabled :: Bool
  , ssBoardSize :: BoardSize
  , ssIndex :: Maybe FHandle
  , ssData :: Maybe FHandle
  }

-- | Storage monad.
type Storage a = StateT StorageState IO a

instance HasMetricsConfig (StateT StorageState IO) where
  isMetricsEnabled = gets ssMetricsEnabled

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
  metricsEnabled <- isMetricsEnabled
  let initState = StorageState lts metrics metricsEnabled bsize indexHandle dataHandle
  liftIO $ evalStateT actions initState
  
