{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parallel where

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import Data.Typeable
import Data.Ord
import Data.List
import Data.Aeson
import Text.Printf
import System.Clock

import Types
import Board
import BoardMap

data Processor key input output = Processor (input -> key) (Chan input) (Chan (key, output))

runProcessor :: Int -> (input -> key) -> (input -> IO output) -> IO (Processor key input output)
runProcessor nThreads getKey fn = do
    inputChan <- newChan
    outChan <- newChan

    forM_ [1 .. nThreads] $ \i -> do
       forkIO $ worker inputChan outChan
    return $ Processor getKey inputChan outChan
  where
    worker inChan outChan = forever $ do
      input <- readChan inChan
      output <- fn input
      writeChan outChan (getKey input, output)

process :: Ord key => Processor key input output -> [input] -> IO [output]
process (Processor getKey inChan outChan) inputs = do
    let n = length inputs
    forM_ inputs $ writeChan inChan

    results <- replicateM n $ readChan outChan
    let m = M.fromList results
    forM inputs $ \input -> do
      let output = fromJust $ M.lookup (getKey input) m
      return output




