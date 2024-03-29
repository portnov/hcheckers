{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module AI.Session where

import Control.Monad
import Control.Monad.Except
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Map as M
import System.Random

import Core.Types
import Core.Board

newAiSession :: Checkers (AiSessionId, AiSession)
newAiSession = do
  var <- askSupervisor
  signal <- liftIO newEmptyMVar
  resultVar <- liftIO newEmptyMVar
  sessionId <- liftIO randomIO
  let newSession = AiSession signal resultVar
  liftIO $ atomically $ modifyTVar var $ \st -> st {
                          ssAiSessions = M.insert sessionId newSession (ssAiSessions st)
                        }
  return (sessionId, newSession)

releaseAiSession :: AiSessionId -> Checkers ()
releaseAiSession sessionId = do
  var <- askSupervisor
  ok <- liftIO $ atomically $ stateTVar var $ \st -> do
            case M.lookup sessionId (ssAiSessions st) of
              Nothing -> (False, st)
              Just session ->
                let st' = st {
                            ssAiSessions = M.delete sessionId (ssAiSessions st)
                          }
                in (True, st')
  when (not ok) $
    throwError NoSuchAiSession

returnBoard :: AiSession -> Board -> Checkers ()
returnBoard session board =
    liftIO $ putMVar (aiResult session) board
      
signalStopAiSession :: AiSessionId -> Checkers ()
signalStopAiSession sessionId = do
  var <- askSupervisor
  st <- liftIO $ readTVarIO var
  case M.lookup sessionId (ssAiSessions st) of
    Nothing -> throwError NoSuchAiSession
    Just session -> liftIO $ void $ tryPutMVar (aiStopSignal session) ()

getAiSessionStatus :: AiSessionId -> Checkers AiSessionStatus
getAiSessionStatus sessionId = do
  var <- askSupervisor
  st <- liftIO $ readTVarIO var
  case M.lookup sessionId (ssAiSessions st) of
    Nothing -> return NoAiHere
    Just state -> do
      check <- liftIO $ tryTakeMVar (aiResult state)
      case check of
        Nothing -> return AiRunning
        Just board -> return $ AiDone (boardRep board)

