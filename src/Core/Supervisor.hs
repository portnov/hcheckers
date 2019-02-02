{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

{- 
 - Supervisor is a singleton unit, which manages set of games (which may be
 - just created, running, or already finished). It supports methods like "make this move in that game".
 -}

module Core.Supervisor where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Text.Format.Heavy
import Data.Default
import Data.Aeson hiding (Error)
import Data.Dynamic
import GHC.Generics
import System.Random
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Log.Heavy.Format
import System.Log.FastLogger.Date

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Game
import AI.AlphaBeta () -- import instances only
import AI.AlphaBeta.Types
import Formats.Types
import Formats.Fen
import Formats.Pdn

import Rules.Russian
import Rules.Simple
import Rules.English
import Rules.International
import Rules.Canadian
import Rules.Spancirety
import Rules.Diagonal

-- | Request for new game creation
data NewGameRq = NewGameRq {
    rqRules :: String         -- ^ Rules identifier
  , rqRulesParams :: Value    -- ^ Rules parameters (no rules support parameters atm)
  , rqBoard :: Maybe BoardRep -- ^ Initial board, Nothing for default one
  , rqFen :: Maybe T.Text     -- ^ Initial board in FEN notation
  , rqPdn :: Maybe T.Text     -- ^ Initial board in PDN notation
  , rqPrevBoard :: Maybe GameId
  }
  deriving (Eq, Show, Generic)

-- | Request for attaching AI to the game.
-- Parameter is identifier of AI implementation.
-- Currently there is only one, named 'default'.
data AttachAiRq = AttachAiRq String Value
  deriving (Eq, Show, Generic)

-- | Response to the client.
-- Contains payload and list of notification messages.
data Response = Response RsPayload [Notify]
  deriving (Eq, Show, Generic)

-- | Response payload
data RsPayload =
    NewGameRs GameId Side
  | RegisterUserRs
  | AttachAiRs
  | RunGameRs
  | PollRs [Notify]
  | LobbyRs [Game]
  | NotationRs BoardSize BoardOrientation [(Label, Notation)]
  | StateRs BoardRep GameStatus Side
  | HistoryRs [HistoryRecordRep]
  | PossibleMovesRs [MoveRep]
  | MoveRs BoardRep
  | UndoRs BoardRep
  | CapitulateRs
  | DrawRqRs
  | DrawAcceptRs Bool
  deriving (Eq, Show, Generic)

-- | Create supervisor handle
mkSupervisor :: IO SupervisorHandle
mkSupervisor = do
  var <- atomically $ newTVar $ SupervisorState M.empty 0 M.empty
  return var

-- | List of supported rules with their identifiers
supportedRules :: [(String, SomeRules)]
supportedRules =
  [("russian", SomeRules russian),
   ("simple", SomeRules simple),
   ("english", SomeRules english),
   ("international", SomeRules international),
   ("canadian", SomeRules canadian),
   ("spancirety", SomeRules spancirety),
   ("diagonal", SomeRules diagonal)]

-- | Select rules by client request.
selectRules :: NewGameRq -> Maybe SomeRules
selectRules (NewGameRq {rqRules=name, rqRulesParams=params, rqPdn=mbPdn}) =
    fromPdn mbPdn `mplus` go supportedRules
  where
    -- extract rules from client request field
    go :: [(String, SomeRules)] -> Maybe SomeRules
    go [] = Nothing
    go ((key, (SomeRules rules)) : other)
      | key == name = Just $ SomeRules $ updateRules rules params
      | otherwise = go other

    -- extract rules (GameType tag) from PDN
    fromPdn :: Maybe T.Text -> Maybe SomeRules
    fromPdn Nothing = Nothing
    fromPdn (Just text) =
      case parsePdn Nothing text of
        Left _ -> Nothing
        Right gr -> rulesFromTags (grTags gr)

-- | List of supported AI implementations
supportedAis :: [(String, SomeRules -> SomeAi)]
supportedAis = [("default", \(SomeRules rules) -> SomeAi (AlphaBeta def rules (dfltEvaluator rules)))]

-- | Select AI implementation by client request
selectAi :: AttachAiRq -> SomeRules -> Maybe SomeAi
selectAi (AttachAiRq name params) rules = go supportedAis
  where
    go :: [(String, SomeRules -> SomeAi)] -> Maybe SomeAi
    go [] = Nothing
    go ((key, fn) : other)
      | key == name = Just $ updateSomeAi (fn rules) params
      | otherwise = go other

-- | Initialize AI storage.
-- There should be exactly one AI storage instance running
-- per (AI implementation, game rules) tuple, shared by all games running.
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

-- | Create a game in the New state
newGame :: SomeRules -> Side -> Maybe BoardRep -> Checkers GameId
newGame r@(SomeRules rules) firstSide mbBoardRep = do
  var <- askSupervisor
  liftIO $ atomically $ do
    st <- readTVar var
    let gameId = ssLastGameId st + 1
    writeTVar var $ st {ssLastGameId = gameId}
    game <- mkGame rules gameId firstSide mbBoardRep
    modifyTVar var $ \st -> st {ssGames = M.insert (show gameId) game (ssGames st)}
    return $ show gameId

-- | Register a user in the game
registerUser :: GameId -> Side -> String -> Checkers ()
registerUser gameId side name = do
    var <- askSupervisor
    res <- liftIO $ atomically $ do
      st <- readTVar var
      case M.lookup gameId (ssGames st) of
        Nothing -> return $ Left $ NoSuchGame gameId
        Just game -> do
          if exists name game
            then return $ Left UserNameAlreadyUsed
            else do
              modifyTVar var $ \st ->
                    st {ssGames = M.update (Just . update name) gameId (ssGames st)}
              return $ Right ()
    case res of
      Right _ -> return ()
      Left err -> throwError err
  where
    update name game
      | side == First = game {gPlayer1 = Just (User name)}
      | otherwise     = game {gPlayer2 = Just (User name)}

    exists name game =
      case (gPlayer1 game, gPlayer2 game) of
        (Just p, Nothing) -> isUser name p
        (Nothing, Just p) -> isUser name p
        (Just p1, Just p2) -> isUser name p1 || isUser name p2
        _ -> False

-- | Attach AI to the game
attachAi :: GameId -> Side -> SomeAi -> Checkers ()
attachAi gameId side (SomeAi ai) = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    rules <- getRules gameId
    board0 <- getInitialBoard gameId
    game <- getGame gameId
    withAiStorage rules ai $ \storage ->
      initAi ai storage gameId board0 (gFirstSide game)
  where
    update game
      | side == First = game {gPlayer1 = Just $ AI ai}
      | otherwise     = game {gPlayer2 = Just $ AI ai}

-- | Switch game to the running state.
-- If the first player is AI, let it make a turn.
runGame :: GameId -> Checkers ()
runGame gameId = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    game <- getGame gameId
    let firstSide = gsSide $ gState game
    letAiMove gameId firstSide Nothing
    return ()
  where
    update game = game {gStatus = Running}

-- | Execute actions within GameM monad
withGame :: GameId -> (SomeRules -> GameM a) -> Checkers a
withGame gameId action = do
  var <- askSupervisor
  res <- liftIO $ atomically $ do
    st <- readTVar var
    case M.lookup gameId (ssGames st) of
      Nothing -> return $ Left $ NoSuchGame gameId
      Just game -> do
        let rules = gRules game
        let (r, game') = runState (runExceptT $ action rules) game
        case r of
          Left err -> return $ Left err
          Right result -> do
            writeTVar var $ st {ssGames = M.insert gameId game' (ssGames st)}
            return $ Right result
  case res of
    Right result -> return result
    Left err -> throwError err

-- | Get game by Id
getGame :: GameId -> Checkers Game
getGame gameId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case M.lookup gameId (ssGames st) of
    Just game -> return game
    Nothing -> throwError $ NoSuchGame gameId

-- | Get game rules by game Id
getRules :: GameId -> Checkers SomeRules
getRules gameId = do
  game <- getGame gameId
  return $ gRules game

getInitialBoard :: GameId -> Checkers Board
getInitialBoard gameId = do
  game <- getGame gameId
  return $ gInitialBoard game

-- | Find a game by participating user name.
-- Returns Just game if there is exactly one such game.
getGameByUser :: String -> Checkers (Maybe Game)
getGameByUser name = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case filter (\g -> isJust (sideByUser' g name)) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

-- | Get all messages pending for specified user.
-- Remove that messages from mailboxes.
getMessages :: String -> Checkers [Notify]
getMessages name = do
  var <- askSupervisor
  liftIO $ atomically $ do
    st <- readTVar var
    let mailboxes = mapMaybe getM (M.elems $ ssGames st)
    messages <- forM mailboxes readAll
    return (concat messages)
  where
    readAll chan = do
      mbMsg <- tryReadTChan chan
      case mbMsg of
        Nothing -> return []
        Just msg -> do
          rest <- readAll chan
          return $ msg : rest

    getM game = do
      case sideByUser' game name of
        Nothing -> Nothing
        Just First -> Just $ gMsgbox1 game
        Just Second -> Just $ gMsgbox2 game
    
-- | Get moves that are possible currently
-- in the specified game for specified side.
getPossibleMoves :: GameId -> Side -> Checkers [MoveRep]
getPossibleMoves gameId side =
    withGame gameId $ \(SomeRules rules) -> do
      currentSide <- gets (gsSide . gState)
      if side /= currentSide
        then throwError NotYourTurn
        else do
          game <- get
          moves <- gamePossibleMoves
          return $ map (moveRep rules side) moves

-- | Execute specified move in specified game and return a new board.
doMove :: GameId -> String -> MoveRep -> Checkers BoardRep
doMove gameId name moveRq = do
  game <- getGame gameId
  side <- sideByUser game name
  GMoveRs board' messages <- withGame gameId $ \_ -> doMoveRepRq side moveRq
  queueNotifications gameId messages
  letAiMove gameId (opposite side) (Just board')
  return $ boardRep board'

-- | Undo last pair of moves in the specified game.
doUndo :: GameId -> String -> Checkers BoardRep
doUndo gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  GUndoRs board' messages <- withGame gameId $ \_ -> doUndoRq side
  queueNotifications gameId messages
  letAiMove gameId side (Just board')
  return $ boardRep board'

doCapitulate :: GameId -> String -> Checkers ()
doCapitulate gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  result <- withGame gameId $ \_ -> doCapitulateRq side
  let messages = [
          ResultNotify (opposite side) side result,
          ResultNotify side side result
        ]
  queueNotifications gameId messages

doDrawRequest :: GameId -> String -> Checkers ()
doDrawRequest gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  withGame gameId $ \_ -> doPostDrawRequest side
  let messages = [
          DrawRqNotify (opposite side) side
        ]
  queueNotifications gameId messages
  mbResult <- aiDrawRequest gameId (opposite side)
  case mbResult of
    Nothing -> return ()
    Just accepted -> doDrawAccept' gameId (opposite side) accepted

doDrawAccept' :: GameId -> Side -> Bool -> Checkers ()
doDrawAccept' gameId side accepted = do
  withGame gameId $ \_ -> doDrawAcceptRq side accepted
  let drawNotify = DrawRsNotify (opposite side) side accepted
      resultNotify = [
          ResultNotify (opposite side) side Draw,
          ResultNotify side side Draw
        ]
  let messages =
        if accepted
          then drawNotify : resultNotify
          else [drawNotify]
  queueNotifications gameId messages

doDrawAccept :: GameId -> String -> Bool -> Checkers ()
doDrawAccept gameId name accepted = do
  game <- getGame gameId
  side <- sideByUser game name
  doDrawAccept' gameId side accepted

-- | Execute actions with AI storage instance.
-- AI storage instance must be initialized beforeahead.
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

-- | Let AI make it's turn.
letAiMove :: GameId -> Side -> Maybe Board -> Checkers Board
letAiMove gameId side mbBoard = do
  board <- case mbBoard of
             Just b -> return b
             Nothing -> do
               (_, _, b) <- withGame gameId $ \_ -> gameState
               return b

  game <- getGame gameId
  case getPlayer game side of
    AI ai -> do

      rules <- getRules gameId
      withAiStorage rules ai $ \storage -> do
        timed "Selecting AI move" $ do
            aiMoves <- chooseMove ai storage gameId side board
            if null aiMoves
              then do
                $info "AI failed to move." ()
                return board
              else do
                i <- liftIO $ randomRIO (0, length aiMoves - 1)
                let aiMove = aiMoves !! i
                $info "AI returned {} move(s), selected: {}" (length aiMoves, show aiMove)
                GMoveRs board' messages <- withGame gameId $ \_ -> doMoveRq side (pmMove aiMove)
                $debug "Messages: {}" (Single $ show messages)
                queueNotifications (getGameId game) messages
                return board'

    _ -> return board

aiDrawRequest :: GameId -> Side -> Checkers (Maybe Bool)
aiDrawRequest gameId side = do
  game <- getGame gameId
  board <- do
           (_, _, b) <- withGame gameId $ \_ -> gameState
           return b
  case getPlayer game side of
    AI ai -> do
      rules <- getRules gameId
      withAiStorage rules ai $ \storage -> do
        result <- decideDrawRequest ai storage side board 
        $info "AI response for draw request: {}" (Single result)
        return $ Just result
    _ -> return Nothing

-- | Get current game state
getState :: GameId -> Checkers RsPayload
getState gameId = do
  (side, status, board) <- withGame gameId $ \_ -> gameState
  return $ StateRs (boardRep board) status side

-- | Get game history
getHistory :: GameId -> Checkers [HistoryRecordRep]
getHistory gameId = do
  withGame gameId $ \_ -> gameHistory

-- | Get current position in specified game in FEN notation
getFen :: GameId -> Checkers T.Text
getFen gameId = do
  (side, _, board) <- withGame gameId $ \_ -> gameState
  return $ showFen (bSize board) $ boardToFen side board

-- | Get specified game record in PDN format
getPdn :: GameId -> Checkers T.Text
getPdn gameId = do
  game <- getGame gameId
  return $ showPdn (gRules game) $ gameToPdn game

-- | Get list of running games with specified rules
-- (Nothing - any rules)
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

-- | Get list of fields notation by rules name.
getNotation :: String -> Checkers (BoardSize, BoardOrientation, [(Label, Notation)])
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
          orientation = boardOrientation rules
      in  (size, orientation, notation)
    

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

sideByUser' :: Game -> String -> Maybe Side
sideByUser' game name =
  case (gPlayer1 game, gPlayer2 game) of
    (Just (User name1), _) | name1 == name -> Just First
    (_, Just (User name2)) | name2 == name -> Just Second
    _ -> Nothing

sideByUser :: Game -> String -> Checkers Side
sideByUser game name =
  case sideByUser' game name of
    Just side -> return side
    Nothing -> throwError NoSuchUserInGame

getSideByUser :: GameId -> String -> Checkers Side
getSideByUser gameId name = do
  game <- getGame gameId
  sideByUser game name

-- | Put notification messages in corresponding mailboxes.
queueNotifications :: GameId -> [Notify] -> Checkers ()
queueNotifications gameId messages = do
    game <- getGame gameId
    liftIO $ atomically $
      forM_ messages $ \message ->
        case nDestination message of
          First  -> writeTChan (gMsgbox1 game) message
          Second -> writeTChan (gMsgbox2 game) message

gameIdFromLogMsg :: LogMessage -> Maybe Variable
gameIdFromLogMsg msg = msum $ map (lookup "game") $ map lcfVariables $ lmContext msg

logRouter :: SupervisorHandle -> Chan LogMessage -> Checkers ()
logRouter supervisor chan = do
    let fmt = "[{thread}] {message}"
    tcache <- liftIO $ newTimeCache simpleTimeFormat
    forever $ do
        msg <- liftIO $ readChan chan
        case gameIdFromLogMsg msg of
          Nothing -> return ()
          Just str -> do

            ftime <- liftIO tcache
            let gameId = show str
                level = show (lmLevel msg)
                src = intercalate "." (lmSource msg)
                notifies = [LogNotify side level src text | side <- [First, Second]]
                text = format fmt $ LogMessageWithTime ftime msg
            queueNotifications gameId notifies

