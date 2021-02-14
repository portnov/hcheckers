
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Battle where

import Control.Monad
import Control.Monad.IO.Class
import Data.List (intercalate)
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Printf
import System.Random
import System.Random.Shuffle

import Core.Types
import Core.Board
import Core.Supervisor
import AI.AlphaBeta.Types

(<->) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
v1 <-> v2 = V.zipWith (-) v1 v2

runTournament :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules)) => rules -> [AlphaBeta rules (EvaluatorForRules rules)] -> Int -> Int -> Checkers ()
runTournament rules ais nMatches nGames = do
  forM_ ais $ \ai ->
    liftIO $ print $ aiToVector ai
  let n = length ais
      idxPairs = [(i,j) | i <- [0..n-1], j <- [i+1 .. n-1]]
      ais' = map SomeAi ais
  idxPairs' <- liftIO $ shuffleM idxPairs
  stats <- forM (take nMatches idxPairs') $ \(i,j) ->
             runMatch (SomeRules rules) (ais' !! i) (ais' !! j) nGames
  forM_ (zip idxPairs' stats) $ \((i,j),(first,second,draw)) -> do
      liftIO $ printf "AI#%d vs AI#%d: First %d, Second %d, Draw %d\n" i j first second draw

  let results1 = [(i, first - second) | ((i,j), (first,second,draw)) <- zip idxPairs' stats]
      results2 = [(j, second - first) | ((i,j), (first,second,draw)) <- zip idxPairs' stats]
      results = M.fromListWith (+) (results1 ++ results2)
  forM_ (M.assocs results) $ \(i, value) -> do
      let ai = ais !! i
          vec = map show $ V.toList (aiToVector ai) ++ [fromIntegral value]
          str = intercalate "," vec
      liftIO $ putStrLn str

runMatch :: SomeRules -> SomeAi -> SomeAi -> Int -> Checkers (Int, Int, Int)
runMatch rules ai1 ai2 nGames = do
    (nFirst, nSecond, nDraw) <- go 0 (0, 0, 0)
    liftIO $ printf "First: %d, Second: %d, Draws(?): %d\n" nFirst nSecond nDraw
    return (nFirst, nSecond, nDraw)
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
  resetAiStorageG gameId First
  resetAiStorageG gameId Second
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
  if i > 100 && boardRepLen board <= 8 && hasKing First board && hasKing Second board
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

variableParameters :: [T.Text]
variableParameters = [
    "mobility_weight", "backyard_weight", "center_weight",
    "opposite_side_weight", "backed_weight", "asymetry_weight",
    "pre_king_weight", "attacked_man_coef", "attacked_king_coef"
  ]

nVariableParameters :: Int
nVariableParameters = length variableParameters

updateObject :: [Pair] -> Value -> Value
updateObject pairs (Object v) = Object $ go pairs v
  where
    go [] v = v
    go ((key, value):pairs) v = go pairs (H.insert key value v)
updateObject _ _ = error "invalid object"

modifyObject :: [(T.Text, ScoreBase)] -> Value -> Value
modifyObject pairs (Object v) = Object $ go pairs v
  where
    go [] v = v
    go ((key, delta):pairs) v =
      let v' = H.insertWith modify key (Number (fromIntegral delta)) v
      in  go pairs v'
    
    modify (Number v1) (Number v2) = Number (v1+v2)
    modify _ _ = error "invalid value in modify"

generateVariation :: ScoreBase -> Value -> IO Value
generateVariation dv params = do
    deltas <- replicateM nVariableParameters $ randomRIO (-dv, dv)
    let pairs = [(key, delta) | (key, delta) <- zip variableParameters deltas]
    return $ modifyObject pairs params

generateAiVariations :: Int -> ScoreBase -> FilePath -> IO ()
generateAiVariations n dv path = do
  r <- decodeFileStrict path
  case r of
    Nothing -> fail "Cannot load initial AI"
    Just initValue -> forM_ [1..n] $ \i -> do
                        value <- generateVariation dv initValue
                        encodeFile (printf "ai_variation_%d.json" i) value

