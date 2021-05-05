{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE FlexibleInstances #-}

module AI.AlphaBeta.Types
  ( AlphaBeta (..),
    AlphaBetaParams (..),
    DepthParams (..),
    CacheKey,
    dpLast,
    Stats (..),
    Bound (..),
    MoveAndScore (..),
    PerBoardData (..),
    BoardDataCache, AIData, CacheValue,
    ScoreMoveInput (..),
    AICacheHandle (..),
    StorageState (..),
    ScoreState (..), ScoreM (..),
    ScoreInput (..), ScoreOutput (..),
    DepthIterationInput (..), DepthIterationOutput,
    AiOutput,
    Storage,
    runStorage
  ) where

import Control.Monad.State as St
import Control.Monad.Reader
import qualified Control.Monad.Metrics as Metrics
import Control.Concurrent.STM
import qualified Data.HashPSQ as PQ
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
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
import qualified Core.AdaptiveMap as AM
import Core.Parallel

-- | Alpha-beta prunning AI engine.
-- It is parametrized by game rules, evaluator
-- and an instance of Evaluator.
data AlphaBeta rules eval = AlphaBeta AlphaBetaParams rules eval
  deriving (Eq, Ord, Show, Typeable)

type AlphaBetaR rules = AlphaBeta rules (EvaluatorForRules rules)

data AlphaBetaParams = AlphaBetaParams {
    abDepth :: Depth
  , abStartDepth :: Maybe Depth
  , abCombinationDepth :: Depth
  , abDynamicDepth :: Depth
  , abDeeperIfBad :: Bool
  , abMovesLowBound :: Int
  , abMovesHighBound :: Int
  , abBaseTime :: Maybe Int
  , abRandomOpeningDepth :: Int
  , abRandomOpeningOptions :: Int
  }
  deriving (Eq, Ord, Show)

instance Default AlphaBetaParams where
  def = AlphaBetaParams {
          abDepth = 2
        , abStartDepth = Nothing
        , abCombinationDepth = 8
        , abDynamicDepth = 8
        , abDeeperIfBad = False
        , abMovesLowBound = 4
        , abMovesHighBound = 8
        , abBaseTime = Nothing
        , abRandomOpeningDepth = 1
        , abRandomOpeningOptions = 1
        }

-- Calculation depth parameters
data DepthParams = DepthParams {
    dpInitialTarget :: Depth
  , dpTarget :: Depth     -- ^ Target depth: how deep we currently want to calculate the tree
  , dpCurrent :: Depth    -- ^ Currently achieved depth
  , dpMax :: Depth        -- ^ Maximum allowed depth
  , dpMin :: Depth        -- ^ Minimum allowed depth
  , dpStaticMode :: Bool
  , dpForcedMode :: Bool
  , dpReductedMode :: Bool
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

dpLast :: DepthParams -> Depth
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

data Bound = Alpha | Beta | Exact
  deriving (Generic, Typeable, Eq, Show)

instance Binary Bound
instance Store Bound

data PerBoardData = PerBoardData {
    itemDepth :: {-# UNPACK #-}  ! Depth
  , itemScore :: {-# UNPACK #-}  ! Score
  , itemBound :: ! Bound
  }
  deriving (Generic, Typeable, Show)

instance Semigroup PerBoardData where
  d1 <> d2
    | itemDepth d1 > itemDepth d2 = d1
    | otherwise =  d2

instance Monoid PerBoardData where
  mempty = PerBoardData 0 0 Exact

instance Binary PerBoardData
instance Store PerBoardData

type CacheKey = (V.Vector Double, Board)
type CacheValue = PerBoardData

type BoardDataCache = TBoardMap CacheValue
type AIData = TVar (AM.AdaptiveMap (V.Vector Double) BoardDataCache)

-- | Input for the `scoreMove` method
data ScoreMoveInput rules eval = ScoreMoveInput {
    smiAi :: AlphaBeta rules eval
  , smiCache :: AICacheHandle rules eval
  , smiGameId :: GameId
  , smiSide :: ! Side 
  , smiIndex :: ! Int
  , smiDepth :: {-# UNPACK #-} ! DepthParams
  , smiBoard :: {-# UNPACK #-} ! Board
  , smiMove :: {-# UNPACK #-} ! PossibleMove
  , smiAlpha :: {-# UNPACK #-} ! Score
  , smiBeta :: {-# UNPACK #-} ! Score
  }

type MovesMemo = TBoardMap (Maybe [PossibleMove], Maybe [PossibleMove])

data MoveAndScore = MoveAndScore {
    rMove :: {-# UNPACK #-} ! PossibleMove
  , rScore :: {-# UNPACK #-} ! Score
  }
  deriving (Eq, Show, Generic, Typeable)

-- | Handle to the instance of AI storage
-- and related structures
data AICacheHandle rules eval = AICacheHandle {
    aichRules :: rules
  , aichData :: AIData
  , aichJobIndex :: TVar Int
  , aichProcessor ::  Processor [Int] [ScoreMoveInput rules eval] [MoveAndScore]
  , aichPossibleMoves :: MovesMemo
  , aichLastMoveScoreShift :: TVar (M.Map GameId ScoreBase)
  , aichCurrentCounts :: TVar BoardCounts
  }

-- | State for the Storage monad
data StorageState = StorageState {
    ssLogging :: LoggingTState
  , ssMetrics :: Metrics.Metrics
  , ssMetricsEnabled :: Bool
  , ssBoardSize :: BoardSize
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

-- | State of ScoreM monad.
data ScoreState rules eval = ScoreState {
    ssRules :: rules
  , ssEvaluator :: eval
  , ssCache :: BoardDataCache
  , ssGameId :: GameId
  , ssBestScores :: [Score] -- ^ At each level of depth-first search, there is own "best score"
  , ssBestMoves :: M.Map Int MoveAndScore
  , ssStartTime :: TimeSpec -- ^ Start time of calculation
  , ssTimeout :: Maybe TimeSpec -- ^ Nothing for "no timeout"
  }

-- | Input data for scoreAB method.
data ScoreInput = ScoreInput {
    siSide :: Side
  , siDepth :: DepthParams
  , siAlpha :: Score
  , siBeta :: Score
  , siBoard :: Board
  , siPossibleMoves :: Maybe [PossibleMove]
  , siPrevMove :: Maybe PossibleMove
  }

data ScoreOutput = ScoreOutput {
    soScore :: Score
  , soQuiescene :: Bool
  }

-- | ScoreM monad.
type ScoreM rules eval a = StateT (ScoreState rules eval) Checkers a

instance HasMetricsConfig (StateT (ScoreState rules eval) Checkers) where
  isMetricsEnabled = lift isMetricsEnabled

instance HasLogger (StateT (ScoreState rules eval) Checkers) where
  getLogger = lift getLogger

  localLogger logger actions = do
    st <- St.get
    (result, st') <- lift $ localLogger logger $ runStateT actions st
    St.put st'
    return result

instance HasLogContext (StateT (ScoreState rules eval) Checkers) where
  getLogContext = lift getLogContext

  withLogContext frame actions = do
    st <- St.get
    (result, st') <- lift $ withLogContext frame $ runStateT actions st
    St.put st'
    return result

data DepthIterationInput = DepthIterationInput {
    diiParams :: AlphaBetaParams,
    diiMoves :: [PossibleMove],
    diiSortKeys :: Maybe [Score],
    diiPrevResult :: Maybe DepthIterationOutput
  }

type DepthIterationOutput = [MoveAndScore]
type AiOutput = ([PossibleMove], Score)

runStorage :: (GameRules rules, Evaluator eval) => AICacheHandle rules eval -> Storage a -> Checkers a
runStorage handle actions = do
  lts <- asks csLogging
  let bsize = boardSize (aichRules handle)
  metrics <- Metrics.getMetrics
  metricsEnabled <- isMetricsEnabled
  let initState = StorageState lts metrics metricsEnabled bsize
  liftIO $ evalStateT actions initState
  
