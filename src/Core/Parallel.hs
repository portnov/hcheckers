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
import qualified Data.Text as T
import System.Log.Heavy

import Core.Types

data Processor key input output = Processor (input -> key) (Chan (input, Chan (key, Either Error output)))

runProcessor :: Int -> (input -> key) -> (input -> Checkers output) -> Checkers (Processor key input output)
runProcessor nThreads getKey fn = do
    st <- ask
    inputChan <- liftIO newChan

    forM_ [1 .. nThreads] $ \i -> do
       liftIO $ forkIO $ worker st inputChan i
    return $ Processor getKey inputChan
  where
    worker st inChan i = forever $ do
      (input, outChan) <- readChan inChan
      output <- runCheckersT (withLogVariable "thread" (i :: Int) $ fn input) st
      writeChan outChan (getKey input, output)

runProcessor' :: [T.Text] -> (input -> key) -> (T.Text -> input -> Checkers output) -> Checkers (Processor key input output)
runProcessor' labels getKey fn = do
    st <- ask
    inputChan <- liftIO newChan

    forM_ labels $ \label -> do
       liftIO $ forkIO $ worker st inputChan label
    return $ Processor getKey inputChan
  where
    worker st inChan label = forever $ do
      (input, outChan) <- readChan inChan
      output <- runCheckersT (withLogVariable "thread" label $ fn label input) st
      writeChan outChan (getKey input, output)

process :: Ord key => Processor key input output -> [input] -> Checkers [output]
process processor inputs = do
    results <- process' processor inputs
    case sequence results of
      Right outputs -> return outputs
      Left err -> throwError err

process' :: Ord key => Processor key input output -> [input] -> Checkers [Either Error output]
process' (Processor getKey inChan) inputs = do
    let n = length inputs
    outChan <- liftIO newChan
    forM_ inputs $ \input ->
      liftIO $ writeChan inChan (input, outChan)
    results <- replicateM n $ liftIO $ readChan outChan
    let m = M.fromList results
    let results = [fromJust $ M.lookup (getKey input) m | input <- inputs]
    return results

processSingle :: Processor key input output -> input -> Checkers output
processSingle (Processor _ inChan) input = do
    outChan <- liftIO newChan
    liftIO $ writeChan inChan (input, outChan)
    result <- liftIO $ readChan outChan
    case snd result of
      Right output -> return output
      Left err -> throwError err

