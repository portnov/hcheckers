{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}

module AI.AlphaBeta.Persistent
  where

import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.DeepSeq (force)
import Control.Concurrent.STM
import qualified Data.ByteString as B
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector.Instances () -- import instances only
import Data.Hashable
import Data.Store
import Data.Bits.Coded
import Data.Bits.Coding
import Data.Bytes.Put
import System.Posix.Types
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Environment
import System.FilePath
import System.FilePath.Glob
import System.Directory
import System.Clock
import Text.Printf

import Core.Types
import qualified Core.AdaptiveMap as AM
import Core.Board
import AI.AlphaBeta.Types
import qualified Core.HTable as HT

maxPieces :: Integer
maxPieces = 30

evalHashName :: V.Vector Double -> String
evalHashName v = printf "%016x" (hash v) 

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

getCachePath :: GameRules rules => rules -> Checkers FilePath
getCachePath rules = liftIO $ do
  home <- getEnv "HOME"
  let directory = home </> ".cache" </> "hcheckers" </> rulesName rules
  createDirectoryIfMissing True directory
  return directory

newAiData :: Checkers AIData
newAiData = liftIO $ atomically $ newTVar $ AM.empty 2

loadAiData' :: FilePath -> Checkers (V.Vector Double, M.Map BoardHash PerBoardData)
loadAiData' path = do
  sHandle <- askSupervisor
  sState <- liftIO $ atomically $ readTVar sHandle
  bytes <- liftIO $ B.readFile path
  (vec, pairs) <- liftIO $ decodeIO bytes :: Checkers (V.Vector Double, [(BoardHash, PerBoardData)])
  $info "Load AI cache: {} - {} boards" (path, length pairs)
  return (vec, M.fromList pairs)

loadAiData :: GameRules rules => rules -> Checkers AIData
loadAiData rules = do
  cachePath <- getCachePath rules
  aiData <- newAiData
  load <- asks (aiLoadCache . gcAiConfig . csConfig)
  if load
    then do
      paths <- liftIO $ glob (cachePath </> "*.data")
      sHandle <- askSupervisor
      sState <- liftIO $ atomically $ readTVar sHandle
      perEvalPairs <- forM paths $ \path -> do
        bytes <- liftIO $ B.readFile path
        (vec, pairs) <- liftIO $ decodeIO bytes :: Checkers (V.Vector Double, [(BoardHash, PerBoardData)])
        $info "Load AI cache: {} - {} boards" (path, length pairs)
        return (force vec, force pairs)

      let vecs = map fst perEvalPairs
          cachesForEval = map snd perEvalPairs
      htableSize <- asks (aiHtableSize . gcAiConfig . csConfig)
      maps <- liftIO $ forM cachesForEval $ \pairs -> do
                bmap <- HT.new (htableSize * 1024)
                forM pairs $ \(bHash, item) -> do
                  HT.write bmap bHash item
                return bmap
      liftIO $ atomically $ do
        aiData <- newTVar $ AM.fromList 2 $ zip vecs maps
        return aiData
    else
      return aiData

saveAiData :: GameRules rules => rules -> AIData -> Checkers ()
saveAiData rules var = do
  cachePath <- getCachePath rules
  perEvalMap <- liftIO $ atomically $ readTVar var
  forM_ (zip [1..] $ AM.toList perEvalMap) $ \(i, (vec, ht)) -> do
    dirty <- liftIO $ HT.isDirty ht
    when dirty $ do
      start <- liftIO $ getTime RealtimeCoarse
      let path = cachePath </> evalHashName vec ++ ".data"
      boardsData <- liftIO $ HT.toList ht
      let fileData = (vec, boardsData) :: (V.Vector Double, [(BoardHash, PerBoardData)])
      liftIO $ B.writeFile path $ Data.Store.encode fileData
      liftIO $ HT.markClean ht
      end <- liftIO $ getTime RealtimeCoarse
      let dt = diffTimeSpec end start
          dtStr = printf "%d.%d" (sec dt) (nsec dt `div` (1000*1000))
      $info "Save AI cache: {} - {} boards, in {} seconds" (path, length boardsData, dtStr :: String)

