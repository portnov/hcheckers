{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Learn where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM
import qualified Control.Monad.Metrics as Metrics
import Data.Maybe (fromMaybe)
import qualified Data.IntMap.Strict as IM
import Data.Aeson
import Data.Text.Format.Heavy
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Directory
import System.FilePath ((</>))
import System.FilePath.Glob (glob)
import Text.Printf

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Supervisor (newAiSession, newGame)
import AI.AlphaBeta
import AI.AlphaBeta.Types
import AI.AlphaBeta.Cache
-- import AI.AlphaBeta.Persistent
import Formats.Types
import Formats.Pdn

doLearn' :: (GameRules rules, Evaluator eval) => rules -> eval -> AICacheHandle rules eval -> AlphaBetaParams -> GameRecord -> Checkers ()
doLearn' rules eval var params gameRec = do
    sup <- askSupervisor
    supervisor <- liftIO $ atomically $ readTVar sup
    let startBoard = initBoardFromTags supervisor (SomeRules rules) (grTags gameRec)
    let result = resultFromTags $ grTags gameRec
    $info "Initial board: {}; result: {}" (show startBoard, show result)
    forM_ (instructionsToMoves $ grMoves gameRec) $ \moves -> do
        let (endScore, allBoards) = go [] startBoard result moves
        $info "End score: {}" (Single endScore)
  where
    go boards lastBoard (Just result) [] = (resultToScore result, lastBoard : boards)
    go boards lastBoard Nothing [] =
      let score = evalBoard eval First lastBoard
      in  (score, lastBoard : boards)
    go boards board0 mbResult (moveRec : rest) =
      let board1 = case mrFirst moveRec of
                     Nothing -> board0
                     Just rec ->
                      let Right move1 = parseMoveRec rules First board0 rec
                          (board1, _, _) = applyMove rules First move1 board0
                      in board1
          board2 = case mrSecond moveRec of
                     Nothing -> board1
                     Just rec ->
                      let Right move2 = parseMoveRec rules Second board1 rec
                          (board2, _, _) = applyMove rules Second move2 board1
                      in board2
      in  go (board1 : boards) board2 mbResult rest

    resultToScore FirstWin = win
    resultToScore SecondWin = loose
    resultToScore Draw = 0

parseMoveRecM :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Checkers Move
parseMoveRecM rules side board rec =
  case parseMoveRec rules side board rec of
    Right move -> return move
    Left err -> throwError err

data BoardCounters = BoardCounters {
    bcFirst :: LabelMap (Int, Int)
  , bcSecond :: LabelMap (Int, Int)
  , bcFirstWins :: Int
  , bcSecondWins :: Int
  }
  deriving (Show)

printCounters :: BoardCounters -> IO ()
printCounters bc = do
    printf "First wins: %d; Second wins: %d\n" (bcFirstWins bc) (bcSecondWins bc)
    putStrLn "First:"
    put (bcFirst bc)
    putStrLn "Second:"
    put (bcSecond bc)
  where
    put :: LabelMap (Int, Int) -> IO ()
    put cs = do
      forM_ (IM.toList cs) $ \(idx, (plus, minus)) ->
               printf "%s: +%d -%d\n" (show $ unpackIndex idx) plus minus

updateBoardCounters1 :: Maybe GameResult -> Board -> BoardCounters -> BoardCounters
updateBoardCounters1 res board bc =
    bc {
        bcFirst = update First win (bcFirst bc),
        bcSecond = update Second loose (bcSecond bc)
      }
  where
    zero = (0, 0)
    updateLabel d side l cs = IM.alter (\mbP -> Just $ d side (fromMaybe zero mbP)) (labelIndex l) cs
    update side d cs = foldr (updateLabel d side) cs (allMyLabels side board)

    isWin First (Just FirstWin) = True
    isWin Second (Just SecondWin) = True
    isWin _ _ = False

    isLoose First (Just SecondWin) = True
    isLoose Second (Just FirstWin) = True
    isLoose _ _ = False
    
    win side (wins, looses)
      | isWin side res = (wins+1, looses)
      | otherwise = (wins, looses)

    loose side (wins, looses)
      | isLoose side res = (wins, looses+1)
      | otherwise = (wins, looses)

updateBoardCounters :: Maybe GameResult -> [Board] -> BoardCounters -> BoardCounters
updateBoardCounters res boards bc =
  let bc' = foldr (updateBoardCounters1 res) bc boards
      (deltaFirst, deltaSecond) =
        case res of
          Just FirstWin -> (1, 0)
          Just SecondWin -> (0, 1)
          _ -> (0, 0)
  in  bc' {
           bcFirstWins = bcFirstWins bc' + deltaFirst,
           bcSecondWins = bcSecondWins bc' + deltaSecond
          }

doLearn :: (GameRules rules, VectorEvaluator eval)
        => rules
        -> eval
        -> AICacheHandle rules eval
        -> AlphaBetaParams
        -> BoardCounters
        -> GameId
        -> GameRecord
        -> Checkers BoardCounters
doLearn rules eval var params counters gameId gameRec = do
    sup <- askSupervisor
    supervisor <- liftIO $ atomically $ readTVar sup
    let startBoard = initBoardFromTags supervisor (SomeRules rules) (grTags gameRec)
    $info "Initial board: {}; tags: {}" (show startBoard, show $ grTags gameRec)
    (lastBoards, score) <-
        loop (instructionsToMoves $ grMoves gameRec) $ \moves -> do
                    (endScore, allBoards) <- go (0, []) startBoard [] moves
                    $info "End score: {}" (Single endScore)
                    return (allBoards, endScore)
    liftIO $ print $ "N boards:" ++ show (length lastBoards)
    return $ updateBoardCounters (grResult gameRec) lastBoards counters

  where
    loop [] _ = fail "empty games list"
    loop [m] actions = actions m
    loop (m:ms) actions = do
      actions m
      loop ms actions

    go (score, boards) lastBoard _ [] = return (score, lastBoard : boards)
    go (score0, boards) board0 predicted (moveRec : rest) = do
      (board1, predict2, score2) <- do
        case mrFirst moveRec of
          Nothing -> return (board0, [], score0)
          Just rec -> do
            move1 <- parseMoveRecM rules First board0 rec
            if move1 `elem` map pmMove predicted
              then Metrics.increment "learn.hit"
              else Metrics.increment "learn.miss"
            let (board1, _,_) = applyMove rules First move1 board0
            (predict2, score2) <- processMove rules eval var params gameId Second move1 board1
            return (board1, predict2, score2)
      case mrSecond moveRec of
        Nothing -> return (score2, board0 : board1 : boards)
        Just rec -> do
          move2 <- parseMoveRecM rules Second board1 rec
          if move2 `elem` map pmMove predict2
            then Metrics.increment "learn.hit"
            else Metrics.increment "learn.miss"
          let (board2, _, _) = applyMove rules Second move2 board1
          (predict1, score1) <- processMove rules eval var params gameId First move2 board2
          go (score1, board0 : board1 : boards) board2 predict1 rest

processMove :: (GameRules rules, VectorEvaluator eval)
            => rules
            -> eval
            -> AICacheHandle rules eval
            -> AlphaBetaParams
            -> GameId
            -> Side
            -> Move
            -> Board
            -> Checkers ([PossibleMove], Score)
processMove rules eval var params gameId side move board = do
  let ai = AlphaBeta params rules eval
  (sessionId, newSession) <- newAiSession
  (moves, score) <- runAI ai var gameId side newSession board
  $info "Processed: side {}, move: {}, depth: {} => score {}; we think next best moves are: {}" (show side, show move, abDepth params, show score, show moves)
  return (moves, score)

doRulesMatch :: Maybe SomeRules -> SomeRules -> Bool
doRulesMatch Nothing _ = True
doRulesMatch (Just (SomeRules pdnRules)) (SomeRules myRules) = rulesName pdnRules == rulesName myRules

learnPdn :: (GameRules rules, VectorEvaluator eval, ToJSON eval) => AlphaBeta rules eval -> FilePath -> Checkers ()
learnPdn ai@(AlphaBeta params rules eval) path = do
    cache <- loadAiCache scoreMoveGroup ai
    pdn <- liftIO $ parsePdnFile (Just $ SomeRules rules) path
    let n = length pdn
        counters = BoardCounters IM.empty IM.empty 0 0
    counters' <- loop counters (zip [1.. ] pdn) $ \(i, gameRec, cs) -> do
                    let mbPdnRules = rulesFromPdn gameRec
                    if doRulesMatch mbPdnRules (SomeRules rules)
                      then do
                        -- liftIO $ print pdn
                        $info "Processing game {}/{}..." (i :: Int, n)
                        gameId <- newGame (SomeRules rules) First Nothing Nothing
                        cs' <- doLearn rules eval cache params cs gameId gameRec
                        saveAiStorage ai cache
                        return cs'
                      else do
                        $info "Game {}/{} - skip: unmatching rules" (i :: Int, n)
                        return cs
    liftIO $ printCounters counters'
  where
    loop :: BoardCounters -> [(a,b)] -> ((a,b, BoardCounters) -> Checkers BoardCounters) -> Checkers BoardCounters
    loop counters [] _ = return counters
    loop counters ((i, gameRec):rest) actions = do
      counters' <- actions (i, gameRec, counters)
      loop counters' rest actions

learnPdnOrDir :: (GameRules rules, VectorEvaluator eval, ToJSON eval) => AlphaBeta rules eval -> FilePath -> Checkers ()
learnPdnOrDir ai path = do
  pathExists <- liftIO $ doesPathExist path
  if pathExists
    then do
      dirExists <- liftIO $ doesDirectoryExist path
      if dirExists
        then do
          paths <- liftIO $ glob (path </> "*.pdn")
          forM_ paths $ \path -> do
            $info "Processing file: {}" (Single path)
            learnPdn ai path
        else learnPdn ai path
    else
      fail "Specified path not found"

analyzeOpenings :: (GameRules rules, VectorEvaluator eval, ToJSON eval)
    => AlphaBeta rules eval
    -> Int
    -> Checkers ()
analyzeOpenings ai@(AlphaBeta params rules eval) depth = do
    cache <- loadAiCache scoreMoveGroup ai
    sup <- askSupervisor
    supervisor <- liftIO $ atomically $ readTVar sup
    let board = initBoard supervisor rules
    gameId <- newGame (SomeRules rules) First Nothing Nothing

    let analyzeBoard side b prevBoards = do
          let moves = possibleMoves rules side b
          forM_ moves $ \pm -> do
            let (board', _, _) = applyMove rules side (pmMove pm) b
            (_, score) <- processMove rules eval cache params gameId side (pmMove pm) board'
            return ()
            -- forM_ (zip [1..] prevBoards) $ \(i, prevBoard) -> do
            --   let key = mkCacheKey eval prevBoard
            --   updateAiCache key (\v -> v {itemDepth = itemDepth v + i, itemScore = score}) cache
          return moves

        go _ _ _ _ 0 = return ()
        go side b prevMoves prevBoards i = do
          $info "Checking moves sequence: {}" (Single $ show $ reverse $ prevMoves)
          moves <- analyzeBoard side b prevBoards
          forM_ moves $ \pm -> do
            let (board1, _, _) = applyMove rules side (pmMove pm) b
            go (opposite side) board1 (pm : prevMoves) (b : prevBoards) (i-1)

    go First board [] [] (2*depth)
    saveAiStorage ai cache
    return ()

