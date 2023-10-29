
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DataKinds #-}

module Battle where

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Data.Aeson hiding (json)
import Data.Aeson.Types
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Yaml
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Text as T
import Data.Text.Format.Heavy
import Text.Printf
import System.Random
import System.Random.Shuffle
import System.Directory
import System.FilePath
import Network.HTTP.Req
import System.Log.Heavy
import System.Log.Heavy.TH
import Text.URI (mkURI)

import Core.Types hiding (timed)
import Core.Board
import Core.Json () -- instances only
import Core.Supervisor
import Core.Parallel
import Core.Monitoring
import Rest.Types
import Rest.Common
import AI.AlphaBeta.Types
import AI

type AB rules = AlphaBeta rules (EvaluatorForRules rules)

type BattleRunner = SomeRules -> (Int,SomeAi) -> (Int,SomeAi) -> FilePath -> Checkers GameResult

type MatchRunner = Int -> SomeRules -> [(Int,Int)] -> [SomeAi] -> Checkers [(Int, Int, Int)]

data GeneticSettings = GeneticSettings {
    gsRules :: String
  , gsUrls :: [T.Text]
  , gsGenerations :: Int
  , gsGames :: Int
  , gsGenerationSize :: Int
  , gsPrevGeneration :: Int
  , gsBestCount :: Int
  , gsInitFiles :: [FilePath]
  } deriving (Show)

instance FromJSON GeneticSettings where
  parseJSON = withObject "GeneticSettings" $ \v -> GeneticSettings
    <$> v .: "rules"
    <*> v .: "urls"
    <*> v .: "generations"
    <*> v .: "games_per_match"
    <*> v .: "generation_size"
    <*> v .: "keep_previous_generation"
    <*> v .: "select_best"
    <*> v .: "init_files"

(<+>) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
v1 <+> v2 = V.zipWith (+) v1 v2

(<->) :: Num a => V.Vector a -> V.Vector a -> V.Vector a
v1 <-> v2 = V.zipWith (-) v1 v2

scale :: Num a => a -> V.Vector a -> V.Vector a
scale a v = V.map (\x -> a*x) v

norm :: V.Vector Double -> Double
-- norm v = sqrt $ V.sum $ V.map (\x -> x*x) v
norm v = (V.sum $ V.map abs v) / fromIntegral (V.length v)

mix :: V.Vector a -> V.Vector a -> IO (V.Vector a)
mix v1 v2 = do
  t <- randomRIO (0.0, 1.0) :: IO Double
  V.forM (V.zip v1 v2) $ \(x1, x2) -> do
    p <- randomRIO (0.0, 1.0)
    if p < t
      then return x1
      else return x2

cross :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules)) => rules -> (AB rules, AB rules) -> Checkers (AB rules)
cross rules (ai1, ai2) = do
  let v1 = aiToVector ai1
      v2 = aiToVector ai2
  mid <- liftIO $ mix v1 v2
  p <- liftIO $ randomRIO (0.0, 1.0) :: Checkers Double
  v3 <- if p < 0.7
          then return mid
          else do
            let delta = 0.10
            liftIO $ V.forM mid $ \v -> do
                             let a = abs v
                             randomRIO (v - delta*a, v + delta*a)
  let v3' = V.take 3 v1 V.++ V.drop 3 v3
  return $ aiFromVector rules v3'

breed :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules)) => rules -> Int -> Int -> [AB rules] -> Checkers [AB rules]
breed rules nNew nOld ais = do
  let n = length ais
      idxPairs = [(i,j) | i <- [0..n-1], j <- [i+1 .. n-1]]
      nNew' = nNew - nOld
  idxPairs' <- liftIO $ shuffleM idxPairs
  let ais' = [(ais !! i, ais !! j) | (i,j) <- idxPairs']
  new <- mapM (cross rules) $ take nNew' $ cycle ais'
  let old = take nOld ais
  return $ old ++ new

runGeneticsJ :: FilePath -> Checkers [SomeAi]
runGeneticsJ cfgPath = do
  cfg <- liftIO $ decodeYamlFile cfgPath
  matchRunner <- if null (gsUrls cfg)
                    then return $ dumbMatchRunner runBattleLocal
                    else do
                         let process url (gameNr, rules, (i,ai1), (j,ai2), path) = do
                                $info "Battle AI#{} vs AI#{} on {}" (i, j, url)
                                timed ("battle.duration." <> url) $ do
                                  increment ("battle.count." <> url)
                                  runBattleRemote url rules (i,ai1) (j,ai2) path
                         processor <- runProcessor' (gsUrls cfg) getJobKey process
                         return $ mkRemoteRunner processor
  withRules (gsRules cfg) $ \rules -> do
      ais <- forM (gsInitFiles cfg) $ \path -> liftIO $ loadAi "default" rules path
      rs <- runGenetics matchRunner rules (gsGenerations cfg) (gsGenerationSize cfg) (gsPrevGeneration cfg) (gsBestCount cfg) (gsGames cfg) ais
      return $ map SomeAi rs

type BattleProcessor = Processor (Int,Int,Int) (Int, SomeRules, (Int,SomeAi), (Int,SomeAi), FilePath) GameResult

getJobKey (gameNr, rules, (i,ai1), (j,ai2), _) = (gameNr, i,j)

mkRemoteRunner :: BattleProcessor -> MatchRunner
mkRemoteRunner processor nGames rules idxPairs ais = do
  let inputs = [(gameNr, rules, (i, ais !! i), (j, ais !! j), "battle.pdn") | (i,j) <- idxPairs, gameNr <- [1..nGames]]
      keys = map getJobKey inputs
  gameResults <- process processor inputs
  let list = [((i,j), result) | (result, (gameNr, i,j)) <- zip gameResults keys]
      byPair = foldr (\(ij, result) m -> M.insertWith (++) ij [result] m) M.empty list
      groupedResults = [fromJust $ M.lookup ij byPair | ij <- idxPairs]
      totals = [calcMatchStats results | results <- groupedResults]
  forM_ (zip idxPairs totals) $ \((i,j), (first, second, draw)) -> do
    $info "Match: AI#{}: {}, AI#{}: {}, Draws(?): {}" (i, first, j, second, draw)
  return totals

runGenetics :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules))
            => MatchRunner
            -> rules
            -> Int           -- ^ Number of generations
            -> Int           -- ^ Generation size
            -> Int           -- ^ number of items to keep from previous generation
            -> Int           -- ^ Number of best items to select for breeding
            -> Int           -- ^ number of games in each match
            -> [AB rules]
            -> Checkers [AB rules]
runGenetics runMatches rules nGenerations generationSize nOld nBest nGames ais = do
      generation0 <- breed rules generationSize nOld ais
      run 1 generation0
  where
      run n generation = do
        $info "Generation #{}" (Single n)
        withLogVariable "generation" n $ do
          best <- selectBest generation
          saveBest n best
          if n == nGenerations
            then return best
            else do
                generation' <- breed rules generationSize nOld best
                run (n+1) generation'

      saveBest n ais = do
        let dirPath = printf "genetics/%d" n
        liftIO $ createDirectoryIfMissing True dirPath
        forM_ (zip [1..] ais) $ \(i, ai) ->
            liftIO $ Data.Aeson.encodeFile (dirPath </> printf "ai_%d.json" (i::Int)) ai

      nMatches = generationSize

      selectBest generation = do
        results <- runTournamentOlympic runMatches rules generation nMatches nGames nBest
        return results

mapP :: Int -> (input -> Checkers output) -> [input] -> Checkers [output]
mapP 1 fn inputs = mapM fn inputs
mapP nThreads fn inputs = do
    let groups = splitBy nThreads inputs
    vars <- replicateM nThreads $ liftIO $ newEmptyMVar
    forM_ (zip groups vars) $ \(group, var) -> do
      forkCheckers $ do
        rs <- forM group fn
        liftIO $ putMVar var rs
    rsGroups <- forM vars $ \var -> liftIO $ takeMVar var
    return $ concat rsGroups

forMP :: Int -> [input] -> (input -> Checkers output) -> Checkers [output]
forMP nThreads inputs fn = mapP nThreads fn inputs

dumbMatchRunner :: BattleRunner -> MatchRunner
dumbMatchRunner runBattle nGames rules idxPairs ais = do
  let nMatches = length idxPairs
  forMP 1 (zip [1..] idxPairs) $ \(k, (i,j)) -> do
      withLogVariable "match" (printf "%d/%d" (k::Int) nMatches :: String) $
          runMatch runBattle rules (i, ais !! i) (j, ais !! j) nGames

runTournamentDumb :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules))
    => MatchRunner -> rules -> [AlphaBeta rules (EvaluatorForRules rules)] -> Int -> Int -> Checkers (M.Map Int Int)
runTournamentDumb runMatches rules ais nMatches nGames = do
  forM_ ais $ \ai ->
    liftIO $ print $ aiToVector ai
  let n = length ais
      idxPairs = [(i,j) | i <- [0..n-1], j <- [i+1 .. n-1]]
      ais' = map SomeAi ais
  idxPairs' <- liftIO $ shuffleM idxPairs
  stats <- runMatches nGames (SomeRules rules) (take nMatches idxPairs') ais'
  $info "Tournament results:" ()

  let results1 = [(i, first - second) | ((i,j), (first,second,draw)) <- zip idxPairs' stats]
      results2 = [(j, second - first) | ((i,j), (first,second,draw)) <- zip idxPairs' stats]
      results = M.fromListWith (+) (results1 ++ results2)
  forM_ (M.toAscList results) $ \(i, value) -> do
      $info "AI#{} => {}" (i, value)
  return results

runTournamentOlympic :: forall rules. (GameRules rules, VectorEvaluator (EvaluatorForRules rules))
    => MatchRunner
    -> rules
    -> [AB rules]
    -> Int
    -> Int
    -> Int
    -> Checkers [AB rules]
runTournamentOlympic runMatches rules ais0 nMatches nGames nBest = do
    let nSrcAis = length ais0
        logNSrcAis = log (fromIntegral nSrcAis :: Double) :: Double
        log2 = log 2
        logNBest = log (fromIntegral nBest :: Double) :: Double
        nStagesTotal = (floor (logNSrcAis / log2)) :: Int
        nAis = (floor (2 ** fromIntegral nStagesTotal)) :: Int
        nStages = (floor ((logNSrcAis - logNBest) / log2)) :: Int
        ais = zip [1..] $ take nAis ais0
    res <- runStages nStages ais
    return $ map snd res
  where
    runStages :: Int -> [(Int, AB rules)] -> Checkers [(Int, AB rules)]
    runStages 0 items = return items
    runStages n items = do
      $info "Stage #{}" (Single n)
      withLogVariable "stage" n $ do
        let ais = map snd items
            idxs = map fst items
            newIdxs = [0 .. length ais - 1]
            idxsMap = M.fromList $ zip newIdxs idxs
        pairs <- makePairs newIdxs
        matchResults <- runMatches nGames (SomeRules rules) pairs (map SomeAi ais)
        winnerIdxs <- zipWithM detectWinner pairs matchResults
        let winnerOldIdxs = [fromJust $ M.lookup i idxsMap | i <- winnerIdxs]
            winners = [(i, ais !! j) | (i,j) <- zip winnerOldIdxs winnerIdxs]
        runStages (n-1) winners

    makePairs :: [Int] -> Checkers [(Int, Int)]
    makePairs idxs = do
      let n = length idxs
          n2 = n `div` 2
      shuffled <- liftIO $ shuffleM idxs
      let firsts = take n2 shuffled
          seconds = drop n2 shuffled
      return $ zip firsts seconds

    detectWinner (i,j) (nFirstWins, nSecondWins, nDraws)
      | nFirstWins > nSecondWins = return i
      | nSecondWins > nFirstWins = return j
      | otherwise = do
          p <- liftIO $ randomRIO (0, 1) :: Checkers Int
          if p == 0
            then return i
            else return j

runMatch :: BattleRunner -> SomeRules -> (Int, SomeAi) -> (Int, SomeAi) -> Int -> Checkers (Int, Int, Int)
runMatch runBattle rules (i,ai1) (j,ai2) nGames = do
    (nFirst, nSecond, nDraw) <- go 0 (0, 0, 0)
    $info "Match: AI#{}: {}, AI#{}: {}, Draws(?): {}" (i, nFirst, j, nSecond, nDraw)
    return (nFirst, nSecond, nDraw)
  where
    go :: Int -> (Int, Int, Int) -> Checkers (Int, Int, Int)
    go k (first, second, draw)
      | k >= nGames = return (first, second, draw)
      | otherwise = do
          withLogVariable "battle" (printf "%d/%d" (k+1) nGames :: String) $ do
            result <- runBattle rules (i,ai1) (j,ai2) (printf "battle_%d.pdn" k)
            let stats = case result of
                          FirstWin -> (first+1, second, draw)
                          SecondWin -> (first, second+1, draw)
                          Draw -> (first, second, draw+1)
            go (k+1) stats

calcMatchStats :: [GameResult] -> (Int, Int, Int)
calcMatchStats rs = go (0, 0, 0) rs
  where
    go (first, second, draw) [] = (first, second, draw)
    go (first, second, draw) (r : rs) =
      let stats = case r of
                    FirstWin -> (first+1, second, draw)
                    SecondWin -> (first, second+1, draw)
                    Draw -> (first, second, draw+1)
      in go stats rs

runBattleLocal :: BattleRunner
runBattleLocal rules (i,ai1) (j,ai2) path = do
  initAiStorage rules ai1
  let firstSide = First
  gameId <- newGame rules firstSide Nothing Nothing
  registerUser gameId First "AI1"
  registerUser gameId Second "AI2"
  attachAi gameId First "AI1" ai1
  attachAi gameId Second "AI2" ai2
  -- resetAiStorageG gameId First
  -- resetAiStorageG gameId Second
  runGame gameId
  result <- loopGame path gameId (opposite firstSide) 0
  $info "Battle AI#{} vs AI#{}: {}" (i, j, show result)
  return result

hasKing :: Side -> BoardRep -> Bool
hasKing side (BoardRep lst) = any isKing (map snd lst)
  where
    isKing (Piece King s) = s == side
    isKing _ = False

loopGame :: FilePath -> GameId -> Side -> Int -> Checkers GameResult
loopGame path gameId side i = do
  StateRs board status side <- getState gameId
  if (i > 200) || (i > 120 && boardRepLen board <= 8 && hasKing First board && hasKing Second board)
    then do
      $info "Too long a game, probably a draw" ()
      -- pdn <- getPdn gameId
      -- liftIO $ TIO.writeFile path pdn
      return Draw
    else do
      history <- getHistory gameId
--       liftIO $ do
--         print $ head history
--         print board
      case status of
        Ended result -> do
              -- pdn <- getPdn gameId
              -- liftIO $ TIO.writeFile path pdn
              return result
        _ ->  do
              letAiMove False gameId side Nothing
              loopGame path gameId (opposite side) (i+1)

variableParameters :: [T.Text]
variableParameters = [
    "mobility_weight", "backyard_weight", "center_weight",
    "opposite_side_weight", "backed_weight", "asymetry_weight",
    "pre_king_weight", "attacked_man_coef", "attacked_king_coef",
    "border_man_weight", "positional_king_weight", "threat_weight",
    "king_on_key_field_weight"
  ]

nVariableParameters :: Int
nVariableParameters = length variableParameters

updateObject :: [Pair] -> Value -> Value
updateObject pairs (Object v) = Object $ go pairs v
  where
    go [] v = v
    go ((key, value):pairs) v = go pairs (KM.insert key value v)
updateObject _ _ = error "invalid object"

modifyObject :: [(T.Text, ScoreBase)] -> Value -> Value
modifyObject pairs (Object v) = Object $ go pairs v
  where
    go [] v = v
    go ((key, delta):pairs) v =
      let v' = KM.insertWith modify (K.fromText key) (Number (fromIntegral delta)) v
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
                        Data.Aeson.encodeFile (printf "ai_variation_%d.json" i) value

decodeJsonFile :: FromJSON a => FilePath -> IO a
decodeJsonFile path = do
  r <- eitherDecodeFileStrict' path
  case r of
    Left err -> fail err
    Right x -> return x

decodeYamlFile :: FromJSON a => FilePath -> IO a
decodeYamlFile path = do
  r <- Data.Yaml.decodeFileEither path
  case r of
    Left err -> fail (show err)
    Right x -> return x

http_ :: T.Text -> (Url 'Http, Option s)
http_ text =
  case mkURI text of
    Nothing -> error $ "Can't parse URI: " ++ T.unpack text
    Just uri ->
      case useHttpURI uri of
        Nothing -> error $ "Unsupported URI: " ++ T.unpack text
        Just (url, opts) -> (url, opts)

runBattleRemote :: T.Text -> BattleRunner
runBattleRemote baseUrl (SomeRules rules) (i,SomeAi ai1) (j,SomeAi ai2) path = do
  let rq = BattleRq (rulesName rules) (toJSON ai1) (toJSON ai2)
      (url, opts) = http_ baseUrl
  rs <- liftIO $ runReq defaultHttpConfig $
          req POST (url /: "battle" /: "run") (ReqBodyJson rq) jsonResponse opts
  return (responseBody rs)

runBattleRemoteIO :: T.Text -> String -> FilePath -> FilePath -> IO GameResult
runBattleRemoteIO baseUrl rulesName aiPath1 aiPath2 = do
  ai1 <- decodeJsonFile aiPath1
  ai2 <- decodeJsonFile aiPath2
  let rq = BattleRq rulesName ai1 ai2
      (url, opts) = http_ baseUrl 
  rs <- runReq defaultHttpConfig $
          req POST (url /: "battle" /: "run") (ReqBodyJson rq) jsonResponse opts
  return (responseBody rs)

