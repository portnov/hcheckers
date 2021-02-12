
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Battle where

import Control.Monad.IO.Class
import qualified Data.Text.IO as TIO

import Core.Types
import Core.Board
import Core.Supervisor

runBattle :: SomeRules -> SomeAi -> SomeAi -> Checkers ()
runBattle rules ai1 ai2 = do
  initAiStorage rules ai1
  let firstSide = First
  gameId <- newGame rules firstSide Nothing
  registerUser gameId First "AI1"
  registerUser gameId Second "AI2"
  attachAi gameId First ai1
  attachAi gameId Second ai2
  runGame gameId
  loopGame gameId (opposite firstSide)

loopGame :: GameId -> Side -> Checkers ()
loopGame gameId side = do
  StateRs board status side <- getState gameId
  history <- getHistory gameId
  liftIO $ do
    print $ head history
    print board
  if status == Running
    then do
         letAiMove gameId side Nothing
         loopGame gameId (opposite side)
    else do
      liftIO $ print status
      pdn <- getPdn gameId
      liftIO $ TIO.writeFile "battle.pdn" pdn

