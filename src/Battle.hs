
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
import Data.Scientific (fromFloatDigits, toRealFloat)
import Data.Yaml
import Data.Maybe
import Data.List (sortOn)
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
import AI.AlphaBeta.Cache (loadAiCache, resetAiCache)
import AI.AlphaBeta.Types
import AI.AlphaBeta (scoreMoveGroup)
import AI

type AB rules = AlphaBeta rules (EvaluatorForRules rules)

data Candidate rules = Candidate {
    cAI :: AB rules
  , cIndexInGeneration :: Int
  , cName :: String
  , cRating :: Int
  }

instance Show (Candidate rules) where
  show c = cName c ++ "[#" ++ show (cIndexInGeneration c) ++ "]"

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

mix :: forall a. (Random a, Fractional a) => V.Vector a -> V.Vector a -> IO (V.Vector a)
mix v1 v2 = do
  c <- randomRIO (-1.05, 1.05) :: IO a
  -- cs <- V.fromList <$> (replicateM (V.length v1) $ randomRIO (-1.05, 1.05) :: IO [a])
  V.forM (V.zip v1 v2) $ \(x1, x2) -> do
    return $ (1.0 - c) * x1 + c * x2

cross :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules)) => rules -> (Candidate rules, Candidate rules) -> Checkers (Candidate rules)
cross rules (cai1, cai2) = do
  let v1 = aiToVector (cAI cai1)
      v2 = aiToVector (cAI cai2)
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
      ai = aiFromVector rules v3'
  return $ Candidate ai 0 (printf "(%s+%s)" (cName cai1) (cName cai2)) 0

breed :: (GameRules rules, VectorEvaluator (EvaluatorForRules rules)) => rules -> Int -> Int -> [Candidate rules] -> Checkers [Candidate rules]
breed rules nNew nOld cais = do
  let replicatedCais = [c {cName = show i ++ "#" ++ cName c} | c <- cais, i <- [1 .. cRating c]]
      n = length replicatedCais
      idxPairs = [(i,j) | i <- [0..n-1], j <- [i+1 .. n-1], cName (replicatedCais !! i) /= cName (replicatedCais !! j)]
      nNew' = nNew - nOld
  idxPairs' <- liftIO $ shuffleM idxPairs
  let cais' = [(replicatedCais !! i, replicatedCais !! j) | (i,j) <- idxPairs']
  new <- mapM (cross rules) $ take nNew' $ cycle cais'
  let old = take nOld cais
      result = old ++ new
  return [c {cIndexInGeneration = i} | (i, c) <- zip [0..] result]

runGeneticsJ :: FilePath -> Checkers [SomeAi]
runGeneticsJ cfgPath = do
  cfg <- liftIO $ decodeYamlFile cfgPath
  matchRunner <- if null (gsUrls cfg)
                    then return $ dumbMatchRunner runBattleLocal
                    else do
                         let process url (gameNr, rules, (i,ai1), (j,ai2), path) = do
                                withLogVariable "battle" gameNr $ do
                                  $info "Battle AI#{} vs AI#{} on {}" (i, j, url)
                                  timed ("battle.duration." <> url) $ do
                                    increment ("battle.count." <> url)
                                    res <- runBattleRemote url rules (i,ai1) (j,ai2) path
                                    $info "Remote battle AI#{} vs AI#{} on {} => {}" (i, j, url, show res)
                                    return res
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

prepareCandidates :: [AB rules] -> [Candidate rules]
prepareCandidates ais = [Candidate ai i (show i) 1 | (i, ai) <- zip [0..] ais]

runGenetics :: forall rules. (GameRules rules, VectorEvaluator (EvaluatorForRules rules))
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
      let cais = prepareCandidates ais
      generation0 <- breed rules generationSize nOld cais
      result <- run 1 generation0
      return $ map cAI result
  where
      run n generation = do
        cache <- loadAiCache scoreMoveGroup rules
        resetAiCache (cache :: AICacheHandle rules (EvaluatorForRules rules))
        $info "Generation #{}" (Single n)
        withLogVariable "generation" n $ do
          best <- selectBest generation
          saveBest n best
          if n == nGenerations
            then return best
            else do
                generation' <- breed rules generationSize nOld best
                run (n+1) generation'

      saveBest n cais = do
        let dirPath = printf "genetics/%d" n
        liftIO $ createDirectoryIfMissing True dirPath
        forM_ cais $ \cai ->
          liftIO $ Data.Aeson.encodeFile (dirPath </> printf "ai_%s_%d.json" (cName cai) (cRating cai)) (cAI cai)

      selectBest generation = do
        runTournamentOlympic runMatches rules generation nGames nBest

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

calcTournamentResults :: [(Int, Int)] -> [(Int, Int, Int)] -> M.Map Int Int
calcTournamentResults idxPairs matchStats =
  let results1 = [(i, first - second) | ((i,j), (first,second,draw)) <- zip idxPairs matchStats]
      results2 = [(j, second - first) | ((i,j), (first,second,draw)) <- zip idxPairs matchStats]
  in  M.fromListWith (+) (results1 ++ results2)

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

  let results = calcTournamentResults idxPairs' stats
  forM_ (M.toAscList results) $ \(i, value) -> do
      $info "AI#{} => {}" (i, value)
  return results

runTournamentOlympic :: forall rules. (GameRules rules, VectorEvaluator (EvaluatorForRules rules))
    => MatchRunner
    -> rules
    -> [Candidate rules]
    -> Int
    -> Int
    -> Checkers [Candidate rules]
runTournamentOlympic runMatches rules cais0 nGames nBest = do
    let nSrcAis = length cais0
        logNSrcAis = log (fromIntegral nSrcAis :: Double) :: Double
        log2 = log 2
        logNBest = log (fromIntegral nBest :: Double) :: Double
        nStagesTotal = (floor (logNSrcAis / log2)) :: Int
        nAis = (floor (2 ** fromIntegral nStagesTotal)) :: Int
        nStages = (floor ((logNSrcAis - logNBest) / log2)) :: Int
        cais = [c {cRating = 0} | c <- take nAis cais0]
    res <- runStages nStages nStages cais
    let sortedRes = sortOn (\cai -> negate (cRating cai)) res
    return sortedRes
  where
    runStages :: Int -> Int -> [Candidate rules] -> Checkers [Candidate rules]
    runStages _ 0 items = return items
    runStages nStages n items = do
      $info "Stage #{}" (Single n)
      withLogVariable "stage" n $ do
        let ais = map cAI items
            idxs = map cIndexInGeneration items
            ratings = map cRating items
            newIdxs = [0 .. length ais - 1]
            idxsMap = M.fromList $ zip newIdxs idxs
        pairs <- makePairs (nStages /= n) ratings newIdxs
        matchResults <- runMatches nGames (SomeRules rules) pairs (map SomeAi ais)
        winnerIdxs <- zipWithM detectWinner pairs matchResults
        let stageResults = calcTournamentResults pairs matchResults
            ratings = [fromJust $ M.lookup i stageResults | i <- winnerIdxs]
            winnerOldIdxs = [fromJust $ M.lookup i idxsMap | i <- winnerIdxs]
            winners = [(items !! j) {cRating = rating} | (j, rating) <- zip winnerIdxs ratings]
        let winnerInfo c = (printf "%s (rating %d)" (show c) (cRating c)) :: String
        $info "Winners at this stage are: {}" (Single $ show $ map winnerInfo winners)
        runStages nStages (n-1) winners

    makePairs :: Bool -> [Int] -> [Int] -> Checkers [(Int, Int)]
    makePairs doSort ratings idxs = do
      let n = length idxs
          n2 = n `div` 2
          orderedIdxs
            | doSort = map snd $ sortOn (negate . fst) $ zip ratings idxs
            | otherwise = idxs
      let firsts = take n2 orderedIdxs
          seconds = drop n2 orderedIdxs
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
    (nFirst, nSecond, nDraw) <- go False 0 (0, 0, 0)
    $info "Match: AI#{}: {}, AI#{}: {}, Draws(?): {}" (i, nFirst, j, nSecond, nDraw)
    return (nFirst, nSecond, nDraw)
  where
    invertIfNeeded False result = result
    invertIfNeeded True FirstWin = SecondWin
    invertIfNeeded True SecondWin = FirstWin
    invertIfNeeded True Draw = Draw

    go :: Bool -> Int -> (Int, Int, Int) -> Checkers (Int, Int, Int)
    go swap k (first, second, draw)
      | k >= nGames = return (first, second, draw)
      | otherwise = do
          withLogVariable "battle" (printf "%d/%d" (k+1) nGames :: String) $ do
            let (pair1, pair2)
                  | swap      = ((j, ai2), (i, ai1))
                  | otherwise = ((i, ai1), (j, ai2))
            result <- runBattle rules pair1 pair2 (printf "battle_%d.pdn" k)
            let stats = case invertIfNeeded swap result of
                          FirstWin -> (first+1, second, draw)
                          SecondWin -> (first, second+1, draw)
                          Draw -> (first, second, draw+1)
            go (not swap) (k+1) stats

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
    "temp_asymetry_weight",
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

modifyObject :: (Double -> Double -> Double) -> [(T.Text, Double)] -> Value -> Value
modifyObject plus pairs (Object v) = Object $ go pairs v
  where
    go [] v = v
    go ((key, delta):pairs) v =
      let v' = KM.insertWith modify (K.fromText key) (Number (fromFloatDigits delta)) v
          v'' = KM.map roundV v'
      in  go pairs v''
    
    modify (Number v1) (Number v2) = Number $ fromIntegral $ round $ toRealFloat v1 `plus` toRealFloat v2
    modify _ _ = error "invalid value in modify"

    roundV (Number v) = Number $ fromIntegral $ round v
    roundV x = x

generateVariationBased :: Double -> Value -> IO Value
generateVariationBased dv params = do
    coefs <- replicateM nVariableParameters $ randomRIO (0, dv)
    let pairs = [(key, delta) | (key, delta) <- zip variableParameters coefs]
    return $ modifyObject (*) pairs params

generateAiVariationsBased :: Int -> Double -> FilePath -> IO ()
generateAiVariationsBased n dv path = do
  r <- decodeFileStrict path
  case r of
    Nothing -> fail "Cannot load initial AI"
    Just initValue -> forM_ [1..n] $ \i -> do
                        value <- generateVariationBased dv initValue
                        Data.Aeson.encodeFile (printf "ai_variation_%d.json" i) value

generateAiVariationsFromZero :: ScoreBase -> FilePath -> IO ()
generateAiVariationsFromZero maxValue path = do
  r <- decodeFileStrict path
  let n = length variableParameters
      mkPair i key
        | key == variableParameters !! i = (key, fromIntegral maxValue)
        | otherwise = (key, 0.0)
  case r of
    Nothing -> fail "Cannot load initial AI"
    Just initValue -> forM_ [0..n-1] $ \i -> do
                        let pairs = map (mkPair i) variableParameters
                            value' = modifyObject (+) pairs initValue
                        Data.Aeson.encodeFile (printf "ai_variation_%d.json" i) value'

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

