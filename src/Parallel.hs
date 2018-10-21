{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Parallel where

import Control.Monad
import Control.Monad.Reader
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as M

import Types

data Processor key input output = Processor (input -> key) (Chan input) (Chan (key, output))

runProcessor :: Int -> (input -> key) -> (input -> Checkers output) -> Checkers (Processor key input output)
runProcessor nThreads getKey fn = do
    st <- ask
    inputChan <- liftIO newChan
    outChan <- liftIO newChan

    forM_ [1 .. nThreads] $ \i -> do
       liftIO $ forkIO $ worker st inputChan outChan
    return $ Processor getKey inputChan outChan
  where
    worker st inChan outChan = forever $ do
      input <- readChan inChan
      output <- runCheckersT (fn input) st
      writeChan outChan (getKey input, output)

process :: Ord key => Processor key input output -> [input] -> Checkers [output]
process (Processor getKey inChan outChan) inputs = do
    let n = length inputs
    forM_ inputs $ \input ->
      liftIO $ writeChan inChan input
    results <- replicateM n $ liftIO $ readChan outChan
    let m = M.fromList results
    forM inputs $ \input -> do
      let output = fromJust $ M.lookup (getKey input) m
      return output

