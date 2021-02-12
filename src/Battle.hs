
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
import Text.Printf

import Core.Types
import Core.Board
import Core.Supervisor

runMatch :: SomeRules -> SomeAi -> SomeAi -> Int -> Checkers ()
runMatch rules ai1 ai2 nGames = do
    (nFirst, nSecond, nDraw) <- go 0 (0, 0, 0)
    liftIO $ printf "First: %d, Second: %d, Draws(?): %d" nFirst nSecond nDraw
  where
    go :: Int -> (Int, Int, Int) -> Checkers (Int, Int, Int)
    go i (first, second, draw)
      | i >= nGames = return (first, second, draw)
      | otherwise = do
          result <- runBattle rules ai1 ai2 (printf "battle_%d.pdn" i)
          let stats = case result of
                        FirstWin -> (first+1, second, draw)
                        SecondWin -> (first, second+1, draw)
                        Draw -> (first, second, draw+1)
          go (i+1) stats

runBattle :: SomeRules -> SomeAi -> SomeAi -> FilePath -> Checkers GameResult
runBattle rules ai1 ai2 path = do
  initAiStorage rules ai1
  let firstSide = First
  gameId <- newGame rules firstSide Nothing
  registerUser gameId First "AI1"
  registerUser gameId Second "AI2"
  attachAi gameId First ai1
  attachAi gameId Second ai2
  runGame gameId
  result <- loopGame path gameId (opposite firstSide) 0
  liftIO $ print result
  return result

hasKing :: Side -> BoardRep -> Bool
hasKing side (BoardRep lst) = any isKing (map snd lst)
  where
    isKing (Piece King s) = s == side
    isKing _ = False

loopGame :: FilePath -> GameId -> Side -> Int -> Checkers GameResult
loopGame path gameId side i = do
  StateRs board status side <- getState gameId
  if i > 100 && boardRepLen board <= 6 && hasKing First board && hasKing Second board
    then do
      liftIO $ print "Too long a game, probably a draw"
      pdn <- getPdn gameId
      liftIO $ TIO.writeFile path pdn
      return Draw
    else do
      history <- getHistory gameId
      liftIO $ do
        print $ head history
        print board
      case status of
        Ended result -> do
              pdn <- getPdn gameId
              liftIO $ TIO.writeFile path pdn
              return result
        _ ->  do
              letAiMove gameId side Nothing
              loopGame path gameId (opposite side) (i+1)

