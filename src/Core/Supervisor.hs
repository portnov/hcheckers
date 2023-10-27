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
import Control.Monad.Reader
import Control.Monad.Except
import Control.Concurrent
import Control.Concurrent.STM
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Array.IArray as A
import qualified Data.ByteString as B
import Data.Text.Format.Heavy
import Data.Default
import Data.Aeson hiding (Error)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.Types as AT
import Data.Yaml
import Data.Dynamic
import Data.Store
import System.Random hiding (uniform, uniformR)
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Log.Heavy.Format
import System.Log.FastLogger.Date
import System.Random.MWC
import System.Environment
import System.FilePath
import System.Directory
import System.FilePath.Glob (glob)
import System.Clock

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Game
import Core.Json () -- import instances only
import Core.Config
import qualified Core.Timing as Timing
import Rest.Types
import AI.AlphaBeta () -- import instances only
import AI.AlphaBeta.Types
import Formats.Types
import Formats.Fen
import Formats.Pdn

import Rules.Russian
import Rules.Simple
import Rules.English
import Rules.International
import Rules.Brazilian
import Rules.Canadian
import Rules.Spancirety
import Rules.Diagonal
import Rules.Czech
import Rules.Turkish
import Rules.Armenian
import Rules.Frisian
import Rules.Killer

newRandomTable :: IO RandomTable
newRandomTable = do
  random <- withSystemRandom . asGenIO $ \gen ->
    forM [1 .. 4] $ \unboxedPiece ->
      forM [1 .. 16*16] $ \index ->
        uniform gen
  let randomArray = A.listArray ((1,0), (4, 16*16-1)) $ concat random
  return randomArray

loadRandomTable :: IO RandomTable
loadRandomTable = do
  home <- getEnv "HOME"
  let path = home </> ".cache" </> "hcheckers" </> "random.table"
  ex <- doesFileExist path
  if ex
    then do
      bytes <- B.readFile path
      Data.Store.decodeIO bytes
    else do
      table <- newRandomTable
      B.writeFile path $ Data.Store.encode table
      return table

-- | Create supervisor handle
mkSupervisor :: IO SupervisorHandle
mkSupervisor = do
  randomArray <- loadRandomTable
  var <- atomically $ newTVar $ SupervisorState M.empty 0 M.empty M.empty $! randomArray
  return var

-- | List of supported rules with their identifiers
supportedRules :: [(String, SomeRules)]
supportedRules =
  [("russian", SomeRules russian),
   ("simple", SomeRules simple),
   ("english", SomeRules english),
   ("international", SomeRules international),
   ("brazilian", SomeRules brazilian),
   ("canadian", SomeRules canadian),
   ("spancirety", SomeRules spancirety),
   ("diagonal", SomeRules diagonal),
   ("czech", SomeRules czech),
   ("turkish",  SomeRules turkish),
   ("armenian",  SomeRules armenian),
   ("frisian", SomeRules frisian),
   ("killer", SomeRules killer)
   ]

-- | Select rules by client request.
selectRulesEither :: NewGameRq -> Either String SomeRules
selectRulesEither (NewGameRq {rqRules=name, rqRulesParams=params, rqBoard = (PdnBoard pdnText)}) =
    fromPdn pdnText
  where
    -- extract rules (GameType tag) from PDN
    fromPdn :: T.Text -> Either String SomeRules
    fromPdn text =
      case parsePdn Nothing text of
        Left err -> Left err
        Right gr ->
          case rulesFromTags (grTags gr) of
            Nothing -> Left "unsupported rules"
            Just rules -> Right rules
selectRulesEither (NewGameRq {rqRules=name, rqRulesParams=params}) = 
    go supportedRules
  where
    -- extract rules from client request field
    go :: [(String, SomeRules)] -> Either String SomeRules
    go [] = Left "unsupported rules"
    go ((key, (SomeRules rules)) : other)
      | key == name = Right $ SomeRules $ updateRules rules params
      | otherwise = go other

selectRules :: NewGameRq -> Maybe SomeRules
selectRules rq =
  case selectRulesEither rq of
    Left _ -> Nothing
    Right rules -> Just rules

selectRulesEither' :: String -> Either String SomeRules
selectRulesEither' name = selectRulesEither $ NewGameRq {
    rqRules = name,
    rqRulesParams = Null,
    rqTimingControl = Nothing,
    rqBoard = DefaultBoard
  }

selectRules' :: String -> Maybe SomeRules
selectRules' name =
  case selectRulesEither' name of
    Left _ -> Nothing
    Right rules -> Just rules

withRules :: String -> (forall rules. GameRules rules => rules -> a) -> a
withRules name fn =
  case selectRules' name of
    Just (SomeRules rules) -> fn rules
    _ -> error "unknown rules"

-- | List of supported AI implementations
supportedAis :: [(T.Text, SomeRules -> SomeAi)]
supportedAis = [("default", \(SomeRules rules) -> SomeAi (AlphaBeta def rules (dfltEvaluator rules)))]

isCustomAiSettingsEnabled :: Checkers Bool
isCustomAiSettingsEnabled = asks (aiEnableCustomSettings . gcAiConfig . csConfig)

-- | Select AI implementation by client request
selectAi :: AttachAiRq -> SomeRules -> Checkers (Maybe (SomeAi, UserName))
selectAi (AttachAiRq impl name useServerParams params) rules =
    if useServerParams
    then do
      mbSettings <- loadAiSetting impl name
      case mbSettings of
        Nothing -> return Nothing
        Just personality -> return $ go (aipSettings personality) supportedAis
    else do
      enableCustomSetings <- isCustomAiSettingsEnabled
      if enableCustomSetings
        then return $ go params supportedAis
        else throwError CustomAiSettingsDisabled
  where
    go :: Value -> [(T.Text, SomeRules -> SomeAi)] -> Maybe (SomeAi, UserName)
    go params [] = Nothing
    go params ((key, fn) : other)
      | key == impl = Just (updateSomeAi (fn rules) params, name)
      | otherwise = go params other

-- | Initialize AI storage.
-- There should be exactly one AI storage instance running
-- per (AI implementation, game rules) tuple, shared by all games running.
initAiStorage :: SomeRules -> SomeAi -> Checkers ()
initAiStorage (SomeRules rules) (SomeAi ai) = do
  var <- askSupervisor
  let key = (rulesName rules, aiName ai)
  storage <- createAiStorage ai

  liftIO $ atomically $ do
    st <- readTVar var
    case M.lookup key (ssAiStorages st) of
      Nothing -> do
        modifyTVar var $ \st ->
          st {ssAiStorages = M.insert key (toDyn storage) (ssAiStorages st)}
      Just _ -> return ()

-- initAiStorage_ :: SomeRules -> SomeAi -> Checkers ()
-- initAiStorage_ (SomeRules rules) (SomeAi ai) = do
--   var <- askSupervisor
--   let key = (rulesName rules, aiName ai)
--   storage <- createAiStorage ai
--   liftIO $ atomically $ modifyTVar var $ \st ->
--           st {ssAiStorages = M.insert key (toDyn storage) (ssAiStorages st)}
--   return ()

getTimingConfig :: T.Text -> Checkers TimingConfig
getTimingConfig name = do
  configs <- getTimingOptions
  case M.lookup name configs of
    Just config -> return config
    Nothing -> throwError NoSuchTimingConfig

-- | Create a game in the New state
newGame :: SomeRules -> Side -> Maybe BoardRep -> Maybe T.Text -> Checkers GameId
newGame r@(SomeRules rules) firstSide mbBoardRep mbTimingConfigName = do
  var <- askSupervisor
  mbTcfg <- case mbTimingConfigName of
              Nothing -> return Nothing
              Just name -> Just <$> getTimingConfig name
  r <- liftIO $ atomically $ do
    st <- readTVar var
    let gameId = ssLastGameId st + 1
    let st' = st {ssLastGameId = gameId}
    writeTVar var st'
    game <- mkGame st' rules gameId firstSide mbBoardRep mbTcfg
    modifyTVar var $ \st -> st {ssGames = M.insert (show gameId) game (ssGames st)}
    return $ show gameId
  now <- liftIO $ getTime Monotonic
  withGame r $ \_ -> Timing.onStartGame now
  return r

setHistory :: GameId -> [HistoryRecord] -> Checkers ()
setHistory gameId history = 
  withGame gameId $ \_ -> setGameHistory history

locateDefaultInitialBoardsDirectory :: IO (Maybe FilePath)
locateDefaultInitialBoardsDirectory = do
  home <- getEnv "HOME"
  let homePath = home </> ".config" </> "hcheckers" </> "initial_positions"
  ex <- doesDirectoryExist homePath
  if ex
    then return $ Just homePath
    else do
      let etcPath = "/etc" </> "hcheckers" </> "initial_positions"
      ex <- doesDirectoryExist etcPath
      if ex
        then return $ Just etcPath
        else return Nothing

getInitialBoardsDirectory :: Checkers (Maybe FilePath)
getInitialBoardsDirectory = do
  mbConfigPath <- asks (gcInitialBoardsDirectory . csConfig)
  case mbConfigPath of
    Just path -> return (Just path)
    Nothing -> liftIO locateDefaultInitialBoardsDirectory

locateDefaultTimingOptionsFile :: IO (Maybe FilePath)
locateDefaultTimingOptionsFile = do
  home <- getEnv "HOME"
  let homePath = home </> ".config" </> "hcheckers" </> "timing.yaml"
  ex <- doesFileExist homePath
  if ex
    then return $ Just homePath
    else do
      let etcPath = "/etc" </> "hcheckers" </> "timing.yaml"
      ex <- doesFileExist etcPath
      if ex
        then return $ Just etcPath
        else return Nothing

getTimingOptionsFile :: Checkers (Maybe FilePath)
getTimingOptionsFile = do
  mbConfigPath <- asks (gcTimingOptionsFile . csConfig)
  case mbConfigPath of
    Just path -> do
      if isAbsolute path
        then return (Just path)
        else do
          mbCfgPath <- liftIO locateConfig
          case mbCfgPath of
            Nothing -> return (Just path)
            Just cfgPath -> return $ Just $ takeDirectory cfgPath </> path
    Nothing -> liftIO locateDefaultTimingOptionsFile

getTimingOptions :: Checkers (M.Map T.Text TimingConfig)
getTimingOptions = do
  mbPath <- getTimingOptionsFile
  case mbPath of
    Nothing -> return M.empty
    Just path -> do
      $debug "Using timing options from file: {}" (Single path)
      r <- liftIO $ decodeFileEither path
      case r of
        Left err -> throwError $ InvalidTimingConfig (show err)
        Right cfg -> return cfg

getInitialBoards :: SomeRules -> Checkers [FilePath]
getInitialBoards (SomeRules rules) = do
  mbRootPath <- getInitialBoardsDirectory
  case mbRootPath of
    Nothing -> return []
    Just rootPath -> do
      let directory = rootPath </> rulesName rules </> "fen"
      paths <- liftIO $ glob (directory </> "*.fen")
      return paths

getRandomInitialBoard :: SomeRules -> Checkers (Maybe BoardRep)
getRandomInitialBoard rules = do
  boardPaths <- getInitialBoards rules
  if null boardPaths
    then return Nothing
    else do
      let n = length boardPaths
      idx <- liftIO $ (withSystemRandom . asGenIO) $ \gen -> uniformR (0, n-1) gen
      let boardPath = boardPaths !! idx
      $info "Using initial board from preset: {}" (Single boardPath)
      fenText <- liftIO $ TIO.readFile boardPath
      case parseFen rules fenText of
        Left err -> throwError $ InvalidBoard err
        Right (_, board) -> return $ Just board

locateDefaultAiSettingsDirectory :: IO (Maybe FilePath)
locateDefaultAiSettingsDirectory = do
  home <- getEnv "HOME"
  let homePath = home </> ".config" </> "hcheckers" </> "ai"
  ex <- doesDirectoryExist homePath
  if ex
    then return $ Just homePath
    else do
      let etcPath = "/etc" </> "hcheckers" </> "ai"
      ex <- doesDirectoryExist etcPath
      if ex
        then return $ Just etcPath
        else return Nothing

getAiSettingsDirectory :: Checkers (Maybe FilePath)
getAiSettingsDirectory = do
  mbConfigPath <- asks (aiSettingsDirectory . gcAiConfig . csConfig)
  case mbConfigPath of
    Just path -> return (Just path)
    Nothing -> liftIO locateDefaultAiSettingsDirectory

parseAiPersonality :: K.Key -> Data.Aeson.Object -> AT.Parser AiPersonality
parseAiPersonality slug v = do
    value <- v .: slug
    withObject "AI settings" (\s -> do
        name <- s .: "name"
        settings <- s .: "settings"
        return $ AiPersonality (K.toText slug) name settings
      ) value

instance FromJSON AiPersonality where
  parseJSON = withObject "AI" $ \v -> do
    case KM.keys v of
      [slug] -> parseAiPersonality slug v
      _ -> fail $ "Invalid AI settings: too many keys or no keys"

instance FromJSON AiPersonalities where
  parseJSON = withObject "AIs" $ \v -> AiPersonalities <$> do
    forM (KM.keys v) $ \slug -> parseAiPersonality slug v

listAiSettings :: T.Text -> Checkers [AiPersonality]
listAiSettings impl = do
    mbSettingsPath <- getAiSettingsDirectory
    case mbSettingsPath of
      Nothing -> return []
      Just rootPath -> do
        let implPath = rootPath </> T.unpack impl ++ ".yaml"
        ex <- liftIO $ doesFileExist implPath
        if ex
          then do
            personalities <- loadAiSettings implPath
            return personalities
          else return []
  where
    loadAiSettings :: FilePath -> Checkers [AiPersonality]
    loadAiSettings path = do
      res <- liftIO $ Data.Yaml.decodeFileEither path
      case res of
        Left err -> do
          $reportError "Can't parse AI settings file: {}: {}" (path, show err)
          return []
        Right (AiPersonalities value) -> return value

loadAiSetting :: T.Text -> T.Text -> Checkers (Maybe AiPersonality)
loadAiSetting impl slug = do
    personalities <- listAiSettings impl
    return $ search personalities
  where
    search [] = Nothing
    search (p : ps)
      | aipSlug p == slug = Just p
      | otherwise = search ps

-- | Register a user in the game
registerUser :: GameId -> Side -> UserName -> Checkers ()
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
attachAi :: GameId -> Side -> UserName -> SomeAi -> Checkers ()
attachAi gameId side name (SomeAi ai) = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
  where
    update game
      | side == First = game {gPlayer1 = Just $ AI name ai}
      | otherwise     = game {gPlayer2 = Just $ AI name ai}

attachSpectator :: GameId -> UserName -> Checkers ()
attachSpectator gameId name = do
    var <- askSupervisor
    res <- liftIO $ atomically $ do
      st <- readTVar var
      case M.lookup gameId (ssGames st) of
        Nothing -> return $ Left $ NoSuchGame gameId
        Just game -> do
          if spectatorExists game
            then return $ Left UserNameAlreadyUsed
            else do
              box <- newTChan
              modifyTVar var $ \st ->
                    st {ssGames = M.update (Just . update box) gameId (ssGames st)}
              return $ Right ()
    case res of
      Right _ -> return ()
      Left err -> throwError err
  where
    spectatorExists game =
      name `M.member` (gSpectatorsMsgBox game)

    update box game =
      game {gSpectatorsMsgBox = M.insert name box (gSpectatorsMsgBox game)}

-- | Switch game to the running state.
-- If the first player is AI, let it make a turn.
runGame :: GameId -> Checkers ()
runGame gameId = do
    var <- askSupervisor
    liftIO $ atomically $ modifyTVar var $ \st -> st {ssGames = M.update (Just . update) gameId (ssGames st)}
    game <- getGame gameId
    let firstSide = gsSide $ gState game
    letAiMove False gameId firstSide Nothing
    return ()
  where
    update game = game {gStatus = Running}

runGameLoop :: GameId -> Checkers ()
runGameLoop gameId = void $ forkCheckers $ go
  where
    go = do
      (side, status, board) <- withGame gameId $ \_ -> gameState
      case status of
        Running -> do
          letAiMove False gameId side Nothing
          go
        _ -> return ()

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
getGameByUser :: UserName -> Checkers (Maybe Game)
getGameByUser name = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case filter (\g -> isJust (sideByUser' g name)) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

-- | Find a game by participating user name.
-- Returns Just game if there is exactly one such game.
getGameByUser' :: UserName -> GameStatus -> Checkers (Maybe Game)
getGameByUser' name status = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case filter (\g -> isJust (sideByUser' g name) && gStatus g == status) (M.elems $ ssGames st) of
    [game] -> return (Just game)
    _ -> return Nothing

getGameByUser_ :: UserName -> Checkers Game
getGameByUser_ name = do
  mbGame <- getGameByUser name
  case mbGame of
    Just game -> return game
    Nothing -> throwError NoSuchUserInGame

-- | Get all messages pending for specified user.
-- Remove that messages from mailboxes.
getMessages :: UserName -> Checkers [Notify]
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
        Nothing -> M.lookup name (gSpectatorsMsgBox game)
        Just First -> Just $ gMsgbox1 game
        Just Second -> Just $ gMsgbox2 game

getTimeLeft :: GameId -> Checkers (Maybe (Seconds, Seconds))
getTimeLeft gameId = do
  enabled <- withGame gameId $ \_ -> Timing.isTimingEnabled
  if enabled
    then do
      now <- liftIO $ getTime Monotonic
      first <- withGame gameId $ \_ -> Timing.getTimeLeft First now
      second <- withGame gameId $ \_ -> Timing.getTimeLeft Second now
      return $ Just (first, second)
    else return Nothing
    
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

afterMove :: GameId -> Side -> Checkers ()
afterMove gameId side = do
  now <- liftIO $ getTime Monotonic
  timeOk <- withGame gameId $ \_ -> Timing.afterMove side now
  when (not timeOk) $ do
    $info "Player #{} ran out of time" (Single $ show side)
    let result = if side == First then SecondWin else FirstWin
    withGame gameId $ \_ -> setGameResult result
    let messages = [
          ResultNotify (opposite side) side result,
          ResultNotify side side result
          ]
    queueNotifications gameId messages
  return ()

-- | Execute specified move in specified game and return a new board.
doMove :: GameId -> UserName -> MoveRep -> Checkers RsPayload
doMove gameId name moveRq = do
  game <- getGame gameId
  side <- sideByUser game name
  GMoveRs board' messages <- withGame gameId $ \_ -> doMoveRepRq side moveRq
  afterMove gameId side
  queueNotifications gameId messages
  sessionId <- letAiMove True gameId (opposite side) (Just board')
  return $ MoveRs (boardRep board') sessionId

-- | Undo last pair of moves in the specified game.
doUndo :: GameId -> UserName -> Checkers (BoardRep, Int)
doUndo gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  GUndoRs board' undoCnt messages <- withGame gameId $ \_ -> doUndoRq side
  queueNotifications gameId messages
  letAiMove False gameId side (Just board')
  return (boardRep board', undoCnt)

doCapitulate :: GameId -> UserName -> Checkers ()
doCapitulate gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  result <- withGame gameId $ \_ -> doCapitulateRq side
  let messages = [
          ResultNotify (opposite side) side result,
          ResultNotify side side result
        ]
  queueNotifications gameId messages

doDrawRequest :: GameId -> UserName -> Checkers (Maybe AiSessionId)
doDrawRequest gameId name = do
  game <- getGame gameId
  side <- sideByUser game name
  withGame gameId $ \_ -> doPostDrawRequest side
  let messages = [
          DrawRqNotify (opposite side) side
        ]
  queueNotifications gameId messages
  mbSessionId <- aiDrawRequest gameId (opposite side)
  return mbSessionId

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

doDrawAccept :: GameId -> UserName -> Bool -> Checkers ()
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

newAiSession :: Checkers (AiSessionId, AiSession)
newAiSession = do
  var <- askSupervisor
  signal <- liftIO newEmptyMVar
  resultVar <- liftIO newEmptyMVar
  sessionId <- liftIO randomIO
  let newSession = AiSession signal resultVar
  liftIO $ atomically $ modifyTVar var $ \st -> st {
                          ssAiSessions = M.insert sessionId newSession (ssAiSessions st)
                        }
  return (sessionId, newSession)

signalStopAiSession :: AiSessionId -> Checkers ()
signalStopAiSession sessionId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case M.lookup sessionId (ssAiSessions st) of
    Nothing -> throwError NoSuchAiSession
    Just session -> liftIO $ void $ tryPutMVar (aiStopSignal session) ()

getAiSessionStatus :: AiSessionId -> Checkers AiSessionStatus
getAiSessionStatus sessionId = do
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  case M.lookup sessionId (ssAiSessions st) of
    Nothing -> return NoAiHere
    Just state -> do
      check <- liftIO $ tryTakeMVar (aiResult state)
      case check of
        Nothing -> return AiRunning
        Just board -> return $ AiDone (boardRep board)

-- | Let AI make it's turn.
letAiMove :: Bool -> GameId -> Side -> Maybe Board -> Checkers (Maybe AiSessionId)
letAiMove separateThread gameId side mbBoard = do
      board <- case mbBoard of
                 Just b -> return b
                 Nothing -> do
                   (_, _, b) <- withGame gameId $ \_ -> gameState
                   return b

      game <- getGame gameId
      case getPlayer game side of
        AI _ ai -> do

          let inThread = if separateThread
                           then \a -> (void $ forkCheckers a)
                           else \a -> (void a)

          rules <- getRules gameId
          withAiStorage rules ai $ \storage -> do
              (sessionId, aiSession) <- newAiSession
              inThread $ do
                (dt, aiMoves) <- timing $ chooseMove ai storage gameId side aiSession board
                if null aiMoves
                  then do
                    $info "AI failed to move." ()
                    return ()
                  else do
                    i <- liftIO $ randomRIO (0, length aiMoves - 1)
                    let aiMove = aiMoves !! i
                    $info "AI returned {} move(s) in {}s, selected: {}" (length aiMoves, showTimeDiff dt, show aiMove)
                    GMoveRs board' messages <- withGame gameId $ \_ -> doMoveRq side (pmMove aiMove)
                    $debug "Messages: {}" (Single $ show messages)
                    queueNotifications (getGameId game) messages
                    liftIO $ putMVar (aiResult aiSession) board'
                afterMove gameId side
                return ()
              return (Just sessionId)

        _ -> return Nothing

askAiMove :: GameId -> Side -> Checkers (AiSessionId, Int)
askAiMove gameId side = do
  game <- getGame gameId
  (board, hintCount) <- withGame gameId $ \_ -> do
                           checkStatus Running
                           checkCurrentSide side
                           b <- gameBoard
                           modify $ increaseHintCount side
                           h <- gets (getHintCount side)
                           return (b, h)
  let side' = opposite side
  case getPlayer game side' of
    AI _ ai -> do
      someRules@(SomeRules rules) <- getRules gameId
      withAiStorage someRules ai $ \storage -> do
          (sessionId, aiSession) <- newAiSession
          forkCheckers $ do
            (dt, aiMoves) <- timing $ chooseMove ai storage gameId side aiSession board
            let moves = map (moveRep rules side . pmMove) aiMoves
            $info "AI returned {} move(s) in {}s" (length aiMoves, showTimeDiff dt)
            let message = AiHintNotify side side moves
            queueNotifications (getGameId game) [message]
          return (sessionId, hintCount)
    _ -> throwError NotAnAi

resetAiStorageG :: GameId -> Side -> Checkers ()
resetAiStorageG gameId side = do
  game <- getGame gameId
  case getPlayer game side of
    AI _ ai -> do
      rules <- getRules gameId
      withAiStorage rules ai $ \storage -> do
        resetAiStorage ai storage
    _ -> fail "User is not an AI"

aiDrawRequest :: GameId -> Side -> Checkers (Maybe AiSessionId)
aiDrawRequest gameId side = do
  game <- getGame gameId
  board <- do
           (_, _, b) <- withGame gameId $ \_ -> gameState
           return b
  case getPlayer game side of
    AI _ ai -> do
      rules <- getRules gameId
      withAiStorage rules ai $ \storage -> do
        (sessionId, aiSession) <- newAiSession
        forkCheckers $ do
          accepted <- decideDrawRequest ai storage gameId side aiSession board 
          $info "AI response for draw request: {}" (Single accepted)
          doDrawAccept' gameId side accepted
        return (Just sessionId)
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

getMoveWithResult :: GameId -> Int -> Side -> Checkers (MoveRep, BoardRep, BoardRep)
getMoveWithResult gameId turnIdx side = do
  withGame gameId $ \_ -> moveWithResult turnIdx side

getInitBoard :: GameId -> Checkers BoardRep
getInitBoard gameId = do
  withGame gameId $ \_ -> gameInitBoard

-- | Get current position in specified game in FEN notation
getFen :: GameId -> Checkers T.Text
getFen gameId = do
  (side, _, board) <- withGame gameId $ \_ -> gameState
  return $ showFen (bSize board) $ boardToFen side board

-- | Get specified game record in PDN format
getPdn :: GameId -> Checkers T.Text
getPdn gameId = do
  game <- getGame gameId
  var <- askSupervisor
  st <- liftIO $ atomically $ readTVar var
  return $ showPdn (gRules game) $ gameToPdn st game

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
getNotation :: String -> Checkers RsPayload
getNotation rname = do
    var <- askSupervisor
    st <- liftIO $ atomically $ readTVar var
    let Just someRules = select supportedRules
        result = process st someRules
    return result

  where
    select [] = Nothing
    select ((name, rules) : rest)
      | name == rname = Just rules
      | otherwise = select rest

    process st (SomeRules rules) =
      let board = initBoard st rules
          labels = labelMapKeys (bAddresses board)
          notation = [(label, boardNotation rules label) | label <- labels]
          size = boardSize rules
          orientation = boardOrientation rules
      in NotationRs size orientation notation (sideNotation rules)
    
getTopology :: String -> Checkers BoardTopology
getTopology rname = do
    var <- askSupervisor
    let Just someRules = select supportedRules
        result = process someRules
    return result

  where
    select [] = Nothing
    select ((name, rules) : rest)
      | name == rname = Just rules
      | otherwise = select rest

    process (SomeRules rules) = boardTopology rules

getPlayer :: Game -> Side -> Player
getPlayer game First = fromJust $ gPlayer1 game
getPlayer game Second = fromJust $ gPlayer2 game

isAI :: Game -> Side -> Bool
isAI game First = case gPlayer1 game of
                    Just (AI _ _) -> True
                    _ -> False
isAI game Second = case gPlayer2 game of
                     Just (AI _ _) -> True
                     _ -> False

sideByUser' :: Game -> UserName -> Maybe Side
sideByUser' game name =
  case (gPlayer1 game, gPlayer2 game) of
    (Just (User name1), _) | name1 == name -> Just First
    (_, Just (User name2)) | name2 == name -> Just Second
    _ -> Nothing

sideByUser :: Game -> UserName -> Checkers Side
sideByUser game name =
  case sideByUser' game name of
    Just side -> return side
    Nothing -> throwError NoSuchUserInGame

getSideByUser :: GameId -> UserName -> Checkers Side
getSideByUser gameId name = do
  game <- getGame gameId
  sideByUser game name

-- | Put notification messages in corresponding mailboxes.
queueNotifications :: GameId -> [Notify] -> Checkers ()
queueNotifications gameId messages = do
    game <- getGame gameId
    liftIO $ atomically $
      forM_ messages $ \message -> do
        forM_ (M.elems $ gSpectatorsMsgBox game) $ \box ->
          writeTChan box message
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

