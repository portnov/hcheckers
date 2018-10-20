{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Supervisor where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
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

data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game
  , ssLastGameId :: Int
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
  var <- atomically $ newTVar $ SupervisorState M.empty 0 M.empty
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
newGame var r@(SomeRules rules) mbBoardRep =
  atomically $ do
    st <- readTVar var
    let gameId = ssLastGameId st + 1
    writeTVar var $ st {ssLastGameId = gameId}
    let game = mkGame rules gameId mbBoardRep
    modifyTVar var $ \st -> st {ssGames = M.insert (show gameId) game (ssGames st)}
    return $ show gameId

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
    letAiMove var gameId First Nothing
    return ()
  where
    update game = game {gStatus = Running}

withGame :: SupervisorHandle -> GameId -> GameM a -> IO a
withGame var gameId action =
    atomically $ do
      st <- readTVar var
      case M.lookup gameId (ssGames st) of
        Nothing -> fail $ "No such game: " ++ gameId
        Just game -> do
          let (r, game') = runState (runExceptT action) game
          case r of
            Left err -> fail err
            Right result -> do
              writeTVar var $ st {ssGames = M.insert gameId game' (ssGames st)}
              return result

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
  moves <- withGame var gameId gamePossibleMoves 
  return $ map (moveRep side) moves

doMove :: SupervisorHandle -> GameId -> String -> MoveRep -> IO BoardRep
doMove var gameId name moveRq = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  let side = fromJust $ sideByUser game name
  GMoveRs board' messages <- withGame var gameId $ doMoveRepRq side moveRq
  queueNotifications var gameId messages
  letAiMove var gameId (opposite side) (Just board')
  return $ boardRep board'

doUndo :: SupervisorHandle -> GameId -> String -> IO BoardRep
doUndo var gameId name = do
  mbGame <- getGame var gameId
  let game = fromJust mbGame
  let side = fromJust $ sideByUser game name
  GUndoRs board' messages <- withGame var gameId $ doUndoRq side
  queueNotifications var gameId messages
  letAiMove var gameId side (Just board')
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

letAiMove :: SupervisorHandle -> GameId -> Side -> Maybe Board -> IO Board
letAiMove var gameId side mbBoard = do
  board <- case mbBoard of
             Just b -> return b
             Nothing -> do
               (_, b) <- withGame var gameId gameState
               return b

  mbGame <- getGame var gameId
  let game = fromJust mbGame
  case getPlayer game side of
    AI ai -> do

      Just rules <- getRules var gameId
      withAiStorage var rules ai $ \storage -> do
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
                GMoveRs board' messages <- withGame var gameId $ doMoveRq side aiMove
                putStrLn $ "Messages: " ++ show messages
                queueNotifications var (getGameId game) messages
                return board'

    _ -> return board

getState :: SupervisorHandle -> GameId -> IO RsPayload
getState var gameId = do
  (side, board) <- withGame var gameId gameState
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

