{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Supervisor where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Ord
import Data.List
import Text.Printf
import Data.Default
import Data.Aeson hiding (Error)
import Data.Dynamic
import GHC.Generics
import System.Random
import System.Clock

import Types
import Board
import Game
import Russian
import AI
import AICache

data Player =
    User String
  | forall ai. GameAi ai => AI ai

instance Show Player where
  show (User name) = name
  show (AI ai) = aiName ai

data GameStatus = New | Running
  deriving (Eq, Show, Generic)

data Game = Game {
    gHandle :: GameHandle
  , gStatus :: GameStatus
  , gRules :: SomeRules
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: [Notify]
  , gMsgbox2 :: [Notify]
  }

instance Show Game where
  show g = printf "<Game %s: %s, 1: %s, 2: %s>"
                  (show $ gHandle g)
                  (show $ gRules g)
                  (show $ gPlayer1 g)
                  (show $ gPlayer2 g)

instance Eq Game where
  g1 == g2 = gHandle g1 == gHandle g2

data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game
  , ssAiStorages :: M.Map (String,String) Dynamic
  }

type SupervisorHandle = TVar SupervisorState

data NewGameRq = NewGameRq String Value (Maybe BoardRep)
  deriving (Eq, Show, Generic)

-- data RegisterUserRq = RegisterUserRq String
--   deriving (Eq, Show, Generic)

data AttachAiRq = AttachAiRq String Value
  deriving (Eq, Show, Generic)

data SupervisorRs = SupervisorRs RsPayload [Notify]
  deriving (Eq, Show, Generic)

data RsPayload =
    NewGameRs GameId
  | RegisterUserRs
  | AttachAiRs
  | RunGameRs
  | PollRs [Notify]
  | LobbyRs [Game]
  | StateRs BoardRep Side
  | PossibleMovesRs [MoveRep]
  | MoveRs BoardRep
  | UndoRs BoardRep
  deriving (Eq, Show, Generic)

mkSupervisor :: IO SupervisorHandle
mkSupervisor = do
  var <- atomically $ newTVar $ SupervisorState M.empty M.empty
  return var

supportedRules :: [(String, SomeRules)]
supportedRules = [("russian", SomeRules Russian)]

selectRules :: NewGameRq -> Maybe SomeRules
selectRules (NewGameRq name params _) = go supportedRules
  where
    go :: [(String, SomeRules)] -> Maybe SomeRules
    go [] = Nothing
    go ((key, (SomeRules rules)) : other)
      | key == name = Just $ SomeRules $ updateRules rules params
      | otherwise = go other

supportedAis :: [(String, SomeRules -> SomeAi)]
supportedAis = [("default", \(SomeRules rules) -> SomeAi (AlphaBeta def rules))]

selectAi :: AttachAiRq -> SomeRules -> Maybe SomeAi
selectAi (AttachAiRq name params) rules = go supportedAis
  where
    go :: [(String, SomeRules -> SomeAi)] -> Maybe SomeAi
    go [] = Nothing
    go ((key, fn) : other)
      | key == name = Just $ updateSomeAi (fn rules) params
      | otherwise = go other

initAiStorage :: SupervisorHandle -> SomeRules -> SomeAi -> IO ()
initAiStorage var (SomeRules rules) (SomeAi ai) = do
  st <- atomically $ readTVar var
  let key = (rulesName rules, aiName ai)
  case M.lookup key (ssAiStorages st) of
    Nothing -> do
      storage <- createAiStorage ai
      atomically $ modifyTVar var $ \st ->
          st {ssAiStorages = M.insert key (toDyn storage) (ssAiStorages st)}
    Just _ -> return ()

newGame :: SupervisorHandle -> SomeRules -> Maybe BoardRep -> IO GameId
newGame var r@(SomeRules rules) mbBoardRep = do
  handle <- spawnGame rules mbBoardRep
  let gameId = show (gThread handle)
  let game = Game handle New r Nothing Nothing [] []
  atomically $ modifyTVar var $ \st -> st {ssGames = M.insert gameId game (ssGames st)}
  return gameId

registerUser :: SupervisorHandle -> GameId -> Side -> String -> IO ()
registerUser var gameId side name =
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update name) gameId (ssGames st)}
  where
    update name game
      | side == First = game {gPlayer1 = Just (User name)}
      | otherwise     = game {gPlayer2 = Just (User name)}

attachAi :: SupervisorHandle -> GameId -> Side -> SomeAi -> IO ()
attachAi var gameId side (SomeAi ai) = do
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
    update game
      | side == First = game {gPlayer1 = Just $ AI ai}
      | otherwise     = game {gPlayer2 = Just $ AI ai}

runGame :: SupervisorHandle -> GameId -> IO ()
runGame var gameId = do
    atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    mbGame <- getGame var gameId
    let game = fromJust mbGame
    letAiMove var game First Nothing
    return ()
  where
    update game = game {gStatus = Running}

getGame :: SupervisorHandle -> GameId -> IO (Maybe Game)
getGame var gameId = do
  st <- atomically $ readTVar var
  return $ M.lookup gameId (ssGames st)

getRules :: SupervisorHandle -> GameId -> IO (Maybe SomeRules)
getRules var gameId = do
  mbGame <- getGame var gameId
  case mbGame of
    Nothing -> return Nothing
    Just game -> return $ Just $ gRules game

getGameByUser :: SupervisorHandle -> String -> IO (Maybe Game)
getGameByUser var name = do
  st <- atomically $ readTVar var
  case filter (\g -> isJust (sideByUser g name)) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

getMessages :: SupervisorHandle -> String -> IO [Notify]
getMessages var name = do
  atomically $ do
    st <- readTVar var
    let messages = concatMap getM (M.elems $ ssGames st)
    let st' = st {ssGames = M.map updateGame (ssGames st)}
    writeTVar var st'
    return messages
  where
    updateGame game =
      case sideByUser game name of
        Nothing -> game
        Just First -> game {gMsgbox1 = []}
        Just Second -> game {gMsgbox2 = []}

    getM game = 
      case sideByUser game name of
        Nothing -> []
        Just First -> gMsgbox1 game
        Just Second -> gMsgbox2 game
    
getPossibleMoves :: SupervisorHandle -> GameId -> Side -> IO [MoveRep]
getPossibleMoves var gameId side = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  writeChan (gInput $ gHandle game) $ GPossibleMovesRq side
  GPossibleMovesRs moves <- readChan (gOutput $ gHandle game)
  return $ map (moveRep side) moves

doMove :: SupervisorHandle -> GameId -> String -> MoveRep -> IO BoardRep
doMove var gameId name moveRq = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  let input = gInput $ gHandle game
  let output = gOutput $ gHandle game
  let side = fromJust $ sideByUser game name
  writeChan input $ DoMoveRepRq side moveRq
  result <- readChan output
  case result of
    Error message -> fail $ "Cannot parse move: " ++ show moveRq ++ ": " ++ message
    DoMoveRs board' messages -> do
      queueNotifications var gameId messages
      letAiMove var game (opposite side) (Just board')
      return $ boardRep board'

doUndo :: SupervisorHandle -> GameId -> String -> IO BoardRep
doUndo var gameId name = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  let input = gInput $ gHandle game
  let output = gOutput $ gHandle game
  let side = fromJust $ sideByUser game name
  writeChan input $ GUndoRq side
  result <- readChan output
  case result of
    Error message -> fail $ "Invalid undo request: " ++ message
    GUndoRs board' messages -> do
      queueNotifications var gameId messages
      letAiMove var game side (Just board')
      return $ boardRep board'

withAiStorage :: GameAi ai
              => SupervisorHandle
              -> SomeRules
              -> ai
              -> (AiStorage ai -> IO a)
              -> IO a
withAiStorage var (SomeRules rules) ai fn = do
    st <- atomically $ readTVar var
    let key = (rulesName rules, aiName ai)
    case M.lookup key (ssAiStorages st) of
      Nothing -> fail $ "AI storage was not initialized yet for key: " ++ show key
      Just value ->
          case fromDynamic value of
            Nothing -> fail $ "AI storage has unexpected type"
            Just storage -> do
              result <- fn storage
              return result

letAiMove :: SupervisorHandle -> Game -> Side -> Maybe Board -> IO Board
letAiMove var game side mbBoard = do
  board <- case mbBoard of
             Just b -> return b
             Nothing -> do
               writeChan (gInput $ gHandle game) GStateRq
               GStateRs _ b <- readChan (gOutput $ gHandle game)
               return b

  case getPlayer game side of
    AI ai -> do

      withAiStorage var (gRules game) ai $ \storage -> do
            time1 <- getTime Realtime
            aiMoves <- chooseMove ai storage side board
            if null aiMoves
              then do
                putStrLn "AI failed to move."
                return board
              else do
                i <- randomRIO (0, length aiMoves - 1)
                let aiMove = aiMoves !! i
                time2 <- getTime Realtime
                let delta = time2-time1
                printf "AI returned %d move(s), selected: %s (in %ds + %dns)\n" (length aiMoves) (show aiMove) (sec delta) (nsec delta)
                writeChan (gInput $ gHandle game) $ DoMoveRq side aiMove
                rs <- readChan (gOutput $ gHandle game)
                case rs of
                  DoMoveRs board' messages -> do
                    putStrLn $ "Messages: " ++ show messages
                    queueNotifications var (getGameId $ gHandle game) messages
                    return board'
                  _ -> fail $ "Unexpected response for move: " ++ show rs

    _ -> return board

getState :: SupervisorHandle -> GameId -> IO RsPayload
getState var gameId = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  writeChan (gInput $ gHandle game) GStateRq
  GStateRs side board <- readChan (gOutput $ gHandle game)
  return $ StateRs (boardRep board) side

getGames :: SupervisorHandle -> Maybe String -> IO [Game]
getGames var mbRulesId = do
  st <- atomically $ readTVar var
  let games = M.elems (ssGames st)
      good (SomeRules rules) =
        case mbRulesId of
          Nothing -> True
          Just rulesId -> rulesName rules == rulesId
  return [game | game <- games, good (gRules game)]

getPlayer :: Game -> Side -> Player
getPlayer game First = fromJust $ gPlayer1 game
getPlayer game Second = fromJust $ gPlayer2 game

isAI :: Game -> Side -> Bool
isAI game First = case gPlayer1 game of
                    Just (AI _) -> True
                    _ -> False
isAI game Second = case gPlayer2 game of
                     Just (AI _) -> True
                     _ -> False

sideByUser :: Game -> String -> Maybe Side
sideByUser game name =
  case (gPlayer1 game, gPlayer2 game) of
    (Just (User name1), _) | name1 == name -> Just First
    (_, Just (User name2)) | name2 == name -> Just Second
    _ -> Nothing

getSideByUser :: SupervisorHandle -> GameId -> String -> IO (Maybe Side)
getSideByUser var gameId name = do
  mbGame <- getGame var gameId
  case mbGame of
    Nothing -> return Nothing
    Just game -> return $ sideByUser game name

queueNotifications :: SupervisorHandle -> GameId -> [Notify] -> IO ()
queueNotifications var gameId messages = atomically $ modifyTVar var go
  where
    go st = foldl route st messages

    route st msg = st {ssGames = M.update (Just . insert msg) gameId (ssGames st)}

    insert msg game
      | nDestination msg == First = game {gMsgbox1 = msg : gMsgbox1 game}
      | otherwise                 = game {gMsgbox2 = msg : gMsgbox2 game}

