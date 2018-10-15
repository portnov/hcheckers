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
import Data.Maybe
import qualified Data.Map as M
import Data.Ord
import Data.List
import Data.Binary
import Data.Typeable
import Data.Default
import Text.Printf
import GHC.Generics
import System.FilePath
import System.Environment
import System.Directory
-- import Database.PostgreSQL.Simple hiding (Binary)
-- import qualified Database.PostgreSQL.Simple as Sql (Binary (..), fromBinary)
-- import Database.PostgreSQL.Simple.FromRow
-- import Database.PostgreSQL.Simple.ToRow
-- import Database.PostgreSQL.Simple.ToField

import Types
import Board
import BoardMap

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
  deriving (Show)

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
  , cisMoves :: [MoveRep]
  }
  deriving (Eq, Show, Generic, Typeable)

instance Binary CacheItemSide

data CacheItem = CacheItem {
    ciFirst :: Maybe CacheItemSide
  , ciSecond :: Maybe CacheItemSide
  }
  deriving (Generic, Typeable)

instance Binary CacheItem

data AIMap = AIMap Bool (BoardMap (M.Map Int CacheItem))

type AICache = TVar AIMap

loadAiCache :: GameRules rules => rules -> AlphaBetaParams -> IO AICache
loadAiCache rules params = do
  var <- if abLoadCache params
            then do
              home <- getEnv "HOME"
              let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
              createDirectoryIfMissing True directory
              let path = directory </> "ai.cache"
              exist <- doesFileExist path
              if exist
                then do
                  putStrLn $ "Loading: " ++ path
                  cache <- decodeFile path
                  atomically $ newTVar $ AIMap False cache
                else atomically $ newTVar $ AIMap False M.empty
            else atomically $ newTVar $ AIMap False M.empty
  forkIO $ cacheDumper rules params var
  return var

cacheDumper :: GameRules rules => rules -> AlphaBetaParams -> AICache -> IO ()
cacheDumper rules params var = 
  when (abSaveCache params) $ forever $ do
    saved <- saveAiCache rules params var
    when (not saved) $ do
      putStrLn "Waiting"
      threadDelay $ 1000 * 1000

saveAiCache :: GameRules rules => rules -> AlphaBetaParams -> AICache -> IO Bool
saveAiCache rules params var = do
      AIMap dirty cache <- atomically $ readTVar var
      if dirty
        then do
          home <- getEnv "HOME"
          let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
          createDirectoryIfMissing True directory
          let path = directory </> "ai.cache"
          putStrLn $ "Saving: " ++ path
          encodeFile path cache
          putStrLn $ "Saved: " ++ path
          atomically $ modifyTVar var $ \(AIMap _ cache) -> AIMap False cache
          return True
        else return False

lookupAiCache :: AlphaBetaParams -> Board -> Int -> Side -> AICache -> IO (Maybe CacheItemSide)
lookupAiCache params board depth side var = do
  let c = boardCounts board
      total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
  if total <= abUseCacheMaxPieces params && depth <= abUseCacheMaxPieces params
    then do
        AIMap _ cache <- atomically $ readTVar var
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

putAiCache :: AlphaBetaParams -> Board -> Int -> Side -> Score -> [Move] -> AICache -> IO ()
putAiCache params board depth side score moves var = do
  let c = boardCounts board
      total = bcFirstMen c + bcSecondMen c + bcFirstKings c + bcSecondKings c
  when (total <= abUpdateCacheMaxPieces params && depth <= abUpdateCacheMaxDepth params) $ do
      atomically $ modifyTVar var $ \(AIMap _ cache) ->
          let updateItem item1 item2 =
                case side of
                  First -> item1 {ciFirst = ciFirst item2 `mplus` ciFirst item1}
                  Second -> item2 {ciSecond = ciSecond item2 `mplus` ciSecond item1}

              updateDepthMap m1 m2 = M.unionWith updateItem m1 m2

              sideItem = CacheItemSide {cisScore = score, cisMoves = map (moveRep side) moves}

              item = case side of
                       First -> CacheItem {ciFirst = Just sideItem, ciSecond = Nothing}
                       Second -> CacheItem {ciFirst = Nothing, ciSecond = Just sideItem}

              init = M.singleton depth item

          in AIMap True $ putBoardMapWith updateDepthMap board init cache

