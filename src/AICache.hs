{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}

module AICache where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.ByteString as B
import Data.Ord
import Data.List
import Data.Binary
import Data.Store
import Data.Typeable
import Data.Default
import Text.Printf
import GHC.Generics
import System.FilePath
import System.Environment
import System.Directory
import System.Clock

import Types
import Board
import BoardMap
import Parallel

data AlphaBeta rules = AlphaBeta AlphaBetaParams rules
  deriving (Eq, Ord, Show, Typeable)

data AlphaBetaParams = AlphaBetaParams {
    abDepth :: Int
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

type AIData = BoardMap (M.Map Int CacheItem)

type ScoreMoveInput rules =
  (AlphaBeta rules, AICacheHandle rules, Side, Int, Board, Move)

data AICache rules = AICache Bool (Processor Move (ScoreMoveInput rules) (Move, Score)) AIData

type AICacheHandle rules = TVar (AICache rules)

loadAiCache :: GameRules rules
            => (ScoreMoveInput rules -> IO (Move, Score))
            -> AlphaBeta rules
            -> IO (AICacheHandle rules)
loadAiCache scoreMove (AlphaBeta params rules) = do
  let getKey (ai, var, side, depth, board, move) = move
  processor <- runProcessor 4 getKey scoreMove
  var <- if abLoadCache params
            then do
              home <- getEnv "HOME"
              let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
              createDirectoryIfMissing True directory
              let path = directory </> "ai.cache"
              exist <- doesFileExist path
              if exist
                then do
                  time1 <- getTime Realtime
                  putStrLn $ "Loading: " ++ path
                  bytes <- B.readFile path
                  cache <- evaluate $ Data.Store.decodeEx bytes
                  time2 <- getTime Realtime
                  let delta = time2-time1
                  printf "Loaded %s in %ds + %dns\n" path (sec delta) (nsec delta)
                  atomically $ newTVar $ AICache False processor cache
                else atomically $ newTVar $ AICache False processor emptyBoardMap
            else atomically $ newTVar $ AICache False processor emptyBoardMap
  forkIO $ cacheDumper rules params var
  return var

cacheDumper :: GameRules rules => rules -> AlphaBetaParams -> AICacheHandle rules -> IO ()
cacheDumper rules params var = 
  when (abSaveCache params) $ forever $ do
    saved <- saveAiCache rules params var
    threadDelay $ 30 * 1000 * 1000

saveAiCache :: GameRules rules => rules -> AlphaBetaParams -> AICacheHandle rules -> IO Bool
saveAiCache rules params var = do
      AICache dirty _ cache <- atomically $ readTVar var
      if dirty
        then do
          home <- getEnv "HOME"
          let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
          createDirectoryIfMissing True directory
          let path = directory </> "ai.cache"
          time1 <- getTime Realtime
          putStrLn $ "Saving: " ++ path
          B.writeFile path $ Data.Store.encode cache
          time2 <- getTime Realtime
          let delta = time2-time1
          printf "Saved %s in %ds + %dns\n" path (sec delta) (nsec delta)
          atomically $ modifyTVar var $ \(AICache _ p cache) -> AICache False p cache
          return True
        else return False

lookupAiCache :: AlphaBetaParams -> Board -> Int -> Side -> AICacheHandle rules -> IO (Maybe CacheItemSide)
lookupAiCache params board depth side var = do
  let c = boardCounts board
      total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
  if total <= abUseCacheMaxPieces params && depth <= abUseCacheMaxPieces params
    then do
        AICache _ _ cache <- atomically $ readTVar var
        case lookupBoardMap board cache of
          Nothing -> return Nothing
          Just byDepth -> do
            let depths = [depth .. depth + abUseCacheMaxDepthPlus params] ++
                         [depth - abUseCacheMaxDepthMinus params .. depth-1] 
            case foldl mplus Nothing [M.lookup d byDepth | d <- depths ] of
              Nothing -> return Nothing
              Just item -> case side of
                             First -> return $ ciFirst item
                             Second -> return $ ciSecond item
    else return Nothing

putAiCache :: AlphaBetaParams -> Board -> Int -> Side -> Score -> [Move] -> AICacheHandle rules -> IO ()
putAiCache params board depth side score moves var = do
  let c = boardCounts board
      total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
  when (total <= abUpdateCacheMaxPieces params && depth <= abUpdateCacheMaxDepth params) $ do
      atomically $ modifyTVar var $ \(AICache _ processor cache) ->
          let updateItem item1 item2 =
                case side of
                  First -> item1 {ciFirst = ciFirst item2 `mplus` ciFirst item1}
                  Second -> item2 {ciSecond = ciSecond item2 `mplus` ciSecond item1}

              updateDepthMap m1 m2 = M.unionWith updateItem m1 m2

              sideItem = CacheItemSide {cisScore = score}

              item = case side of
                       First -> CacheItem {ciFirst = Just sideItem, ciSecond = Nothing}
                       Second -> CacheItem {ciFirst = Nothing, ciSecond = Just sideItem}

              init = M.singleton depth item

          in AICache True processor $ putBoardMapWith updateDepthMap board init cache

