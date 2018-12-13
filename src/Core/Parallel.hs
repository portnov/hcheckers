{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Core.Parallel where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Concurrent
import Data.Maybe
import qualified Data.Map as M
import System.Log.Heavy

import Core.Types

data Processor key input output = Processor (input -> key) (Chan input) (Chan (key, Either Error output))

runProcessor :: Int -> (input -> key) -> (input -> Checkers output) -> Checkers (Processor key input output)
runProcessor nThreads getKey fn = do
    st <- ask
    inputChan <- liftIO newChan
    outChan <- liftIO newChan

    forM_ [1 .. nThreads] $ \i -> do
       liftIO $ forkIO $ worker st inputChan outChan i
    return $ Processor getKey inputChan outChan
  where
    worker st inChan outChan i = forever $ do
      input <- readChan inChan
      output <- runCheckersT (withLogVariable "thread" (i :: Int) $ fn input) st
      writeChan outChan (getKey input, output)

process :: Ord key => Processor key input output -> [input] -> Checkers [output]
process processor inputs = do
    results <- process' processor inputs
    case sequence results of
      Right outputs -> return outputs
      Left err -> throwError err

process' :: Ord key => Processor key input output -> [input] -> Checkers [Either Error output]
process' (Processor getKey inChan outChan) inputs = do
    let n = length inputs
    forM_ inputs $ \input ->
      liftIO $ writeChan inChan input
    results <- replicateM n $ liftIO $ readChan outChan
    let m = M.fromList results
    let results = [fromJust $ M.lookup (getKey input) m | input <- inputs]
    return results

