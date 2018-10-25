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
import Control.Concurrent
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
        , abLoadCache = True
        , abSaveCache = False
        , abUseCacheMaxDepth = 8
        , abUseCacheMaxPieces = 24
        , abUseCacheMaxDepthPlus = 2
        , abUseCacheMaxDepthMinus = 0
        , abUpdateCacheMaxDepth = 6
        , abUpdateCacheMaxPieces = 8
        }

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
  deriving (Generic, Typeable)

instance Binary CacheItem

instance Store CacheItem

type PerBoardData = M.Map Int CacheItem

type AIData = BoardMap PerBoardData

type StorageKey = (BoardKey, Int)

type StorageValue = CacheItemSide

type ScoreMoveInput rules =
  (AlphaBeta rules, AICacheHandle rules, Side, Int, Board, Move)

data AICache rules = AICache {
    aicDirty :: Bool
  , aicProcessor :: Processor Move (ScoreMoveInput rules) (Move, Score)
  , aicData :: AIData
  }

type QueueKey = (BoardCounts, BoardKey)

data AICacheHandle rules = AICacheHandle {
    aichData :: TVar (AICache rules)
  , aichWriteQueue :: WriteQueue
  , aichCleanupQueue :: CleanupQueue
  , aichBlocksCountLock :: QSem
  , aichLockedBoard :: TVar (Maybe BoardKey)
  , aichBoardLock :: QSem
  , aichFileHandle :: Maybe Fd
  }

type WriteQueue = TChan (Board, Int, Side, StorageValue)

type CleanupQueue = TVar (PQ.HashPSQ QueueKey TimeSpec ())

data StorageState rules = StorageState {
    ssLogging :: LoggingTState
  , ssFileOffset :: FileOffset
  , ssHandle :: AICacheHandle rules
  }

type Storage rules a = StateT (StorageState rules) IO a

instance HasLogContext (StateT (StorageState rules) IO) where
  getLogContext = gets (ltsContext . ssLogging)

  withLogContext frame actions = do
    logging <- gets ssLogging
    let logging' = logging {ltsContext = frame : ltsContext logging} 
    modify $ \ss -> ss {ssLogging = logging'}
    result <- actions
    modify $ \ss -> ss {ssLogging = logging}
    return result
    
instance HasLogger (StateT (StorageState rules) IO) where
  getLogger = gets (ltsLogger . ssLogging)

  localLogger logger actions = do
    logging <- gets ssLogging
    let logging' = logging {ltsLogger = logger}
    modify $ \ss -> ss {ssLogging = logging'}
    result <- actions
    modify $ \ss -> ss {ssLogging = logging}
    return result

runStorage :: AICacheHandle rules -> Storage rules a -> Checkers a
runStorage handle actions = do
  lts <- asks csLogging
  let initState = StorageState lts 0 handle
  liftIO $ evalStateT actions initState
  
