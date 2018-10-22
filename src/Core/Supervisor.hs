{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Core.Supervisor where

import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent.STM
import Data.Maybe
import qualified Data.Map as M
import Data.Text.Format.Heavy
import Data.Default
import Data.Aeson hiding (Error)
import Data.Dynamic
import GHC.Generics
import System.Random
import System.Log.Heavy
import System.Log.Heavy.TH

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Game
import AI.AlphaBeta () -- import instances only
import AI.AlphaBeta.Cache -- TODO: get rid of this import

import Rules.Russian
import Rules.Simple
import Rules.International
import Rules.Spancirety
import Rules.Diagonal

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
  | NotationRs BoardSize [(Label, Notation)]
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
supportedRules =
  [("russian", SomeRules Russian),
   ("simple", SomeRules Simple),
   ("international", SomeRules International),
   ("spancirety", SomeRules Spancirety),
   ("diagonal", SomeRules Diagonal)]

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

initAiStorage :: SomeRules -> SomeAi -> Checkers ()
initAiStorage (SomeRules rules) (SomeAi ai) = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  let key = (rulesName rules, aiName ai)
  case M.lookup key (ssAiStorages st) of
    Nothing -> do
      storage <- createAiStorage ai
      liftIO $ atomically $ modifyTVar var $ \st ->
          st {ssAiStorages = M.insert key (toDyn storage) (ssAiStorages st)}
    Just _ -> return ()

newGame :: SomeRules -> Maybe BoardRep -> Checkers GameId
newGame r@(SomeRules rules) mbBoardRep = do
  var <- askSupervisor
  liftIO $ atomically $ do
    st <- readTVar var
    let gameId = ssLastGameId st + 1
    writeTVar var $ st {ssLastGameId = gameId}
    let game = mkGame rules gameId mbBoardRep
    modifyTVar var $ \st -> st {ssGames = M.insert (show gameId) game (ssGames st)}
    return $ show gameId

registerUser :: GameId -> Side -> String -> Checkers ()
registerUser gameId side name = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update name) gameId (ssGames st)}
  where
    update name game
      | side == First = game {gPlayer1 = Just (User name)}
      | otherwise     = game {gPlayer2 = Just (User name)}

attachAi :: GameId -> Side -> SomeAi -> Checkers ()
attachAi gameId side (SomeAi ai) = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
    update game
      | side == First = game {gPlayer1 = Just $ AI ai}
      | otherwise     = game {gPlayer2 = Just $ AI ai}

runGame :: GameId -> Checkers ()
runGame gameId = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    mbGame <- getGame gameId
    let game = fromJust mbGame
    letAiMove gameId First Nothing
    return ()
  where
    update game = game {gStatus = Running}

withGame :: GameId -> GameM a -> Checkers a
withGame gameId action = do
    var <- askSupervisor
    liftIO $ atomically $ do
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

getGame :: GameId -> Checkers (Maybe Game)
getGame gameId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  return $ M.lookup gameId (ssGames st)

getRules :: GameId -> Checkers (Maybe SomeRules)
getRules gameId = do
  mbGame <- getGame gameId
  case mbGame of
    Nothing -> return Nothing
    Just game -> return $ Just $ gRules game

getGameByUser :: String -> Checkers (Maybe Game)
getGameByUser name = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case filter (\g -> isJust (sideByUser g name)) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

getMessages :: String -> Checkers [Notify]
getMessages name = do
  var <- askSupervisor
  liftIO $ atomically $ do
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
    
getPossibleMoves :: GameId -> Side -> Checkers [MoveRep]
getPossibleMoves gameId side = do
  moves <- withGame gameId gamePossibleMoves 
  return $ map (moveRep side) moves

doMove :: GameId -> String -> MoveRep -> Checkers BoardRep
doMove gameId name moveRq = do
  mbGame <- getGame gameId
  let game = fromJust mbGame
  let side = fromJust $ sideByUser game name
  GMoveRs board' messages <- withGame gameId $ doMoveRepRq side moveRq
  queueNotifications gameId messages
  letAiMove gameId (opposite side) (Just board')
  return $ boardRep board'

doUndo :: GameId -> String -> Checkers BoardRep
doUndo gameId name = do
  mbGame <- getGame gameId
  let game = fromJust mbGame
  let side = fromJust $ sideByUser game name
  GUndoRs board' messages <- withGame gameId $ doUndoRq side
  queueNotifications gameId messages
  letAiMove gameId side (Just board')
  return $ boardRep board'

withAiStorage :: GameAi ai
              => SomeRules
              -> ai
              -> (AiStorage ai -> Checkers a)
              -> Checkers a
withAiStorage (SomeRules rules) ai fn = do
    var <- askSupervisor
    st <- liftIO $ atomically $ readTVar var
    let key = (rulesName rules, aiName ai)
    case M.lookup key (ssAiStorages st) of
      Nothing -> fail $ "AI storage was not initialized yet for key: " ++ show key
      Just value ->
          case fromDynamic value of
            Nothing -> fail $ "AI storage has unexpected type"
            Just storage -> do
              result <- fn storage
              return result

letAiMove :: GameId -> Side -> Maybe Board -> Checkers Board
letAiMove gameId side mbBoard = do
  board <- case mbBoard of
             Just b -> return b
             Nothing -> do
               (_, b) <- withGame gameId gameState
               return b

  mbGame <- getGame gameId
  let game = fromJust mbGame
  case getPlayer game side of
    AI ai -> do

      Just rules <- getRules gameId
      withAiStorage rules ai $ \storage -> do
        timed "Selecting AI move" $ do
            aiMoves <- chooseMove ai storage side board
            if null aiMoves
              then do
                $info "AI failed to move." ()
                return board
              else do
                i <- liftIO $ randomRIO (0, length aiMoves - 1)
                let aiMove = aiMoves !! i
                $info "AI returned {} move(s), selected: {}" (length aiMoves, show aiMove)
                GMoveRs board' messages <- withGame gameId $ doMoveRq side aiMove
                $debug "Messages: {}" (Single $ show messages)
                queueNotifications (getGameId game) messages
                return board'

    _ -> return board

getState :: GameId -> Checkers RsPayload
getState gameId = do
  (side, board) <- withGame gameId gameState
  return $ StateRs (boardRep board) side

getGames :: Maybe String -> Checkers [Game]
getGames mbRulesId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  let games = M.elems (ssGames st)
      good (SomeRules rules) =
        case mbRulesId of
          Nothing -> True
          Just rulesId -> rulesName rules == rulesId
  return [game | game <- games, good (gRules game)]

getNotation :: String -> Checkers (BoardSize, [(Label, Notation)])
getNotation rname = do
    let Just someRules = select supportedRules
        result = process someRules
    return result

  where
    select [] = Nothing
    select ((name, rules) : rest)
      | name == rname = Just rules
      | otherwise = select rest

    process (SomeRules rules) =
      let board = initBoard rules
          labels = labelMapKeys (bAddresses board)
          notation = [(label, boardNotation rules label) | label <- labels]
          size = boardSize rules
      in  (size, notation)
    

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

getSideByUser :: GameId -> String -> Checkers (Maybe Side)
getSideByUser gameId name = do
  mbGame <- getGame gameId
  case mbGame of
    Nothing -> return Nothing
    Just game -> return $ sideByUser game name

queueNotifications :: GameId -> [Notify] -> Checkers ()
queueNotifications gameId messages = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var go
  where
    go st = foldl route st messages

    route st msg = st {ssGames = M.update (Just . insert msg) gameId (ssGames st)}

    insert msg game
      | nDestination msg == First = game {gMsgbox1 = msg : gMsgbox1 game}
      | otherwise                 = game {gMsgbox2 = msg : gMsgbox2 game}

