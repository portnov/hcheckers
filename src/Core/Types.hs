{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Types where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Monad.Except
import Control.Monad.Metrics as Metrics
import Control.Concurrent
import Control.Concurrent.STM
import Data.List
import Data.Array.Unboxed
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified STMContainers.Map as SM
import Data.Text.Format.Heavy
import Data.Dynamic
import Data.Aeson (Value)
import Data.Int
import Data.Word
import Data.Binary
import Data.Store
import Data.Default
import Data.Hashable
import Text.Printf
import GHC.Generics
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

import Debug.Trace (traceEventIO)

-- | Label is a coordinate of field on the board.
data Label = Label {
    labelColumn :: ! Line,
    labelRow :: ! Line
  }
  deriving (Eq, Ord, Typeable, Generic)

instance Binary Label where

instance Store Label where
  size = ConstSize 1

  poke (Label col row) = do
    poke $ col * 16 + row

  peek = do
    n <- peek :: Peek Word8
    let row = n `mod` 16
        col = n `div` 16
    return $ Label col row

instance Hashable Label where
  hashWithSalt salt (Label col row) =
    salt `hashWithSalt` col `hashWithSalt` row

-- | Field notation.
type Notation = T.Text

letters :: [Char]
letters = ['a' .. 'z']

instance Show Label where
  show l = letter : show (labelRow l + 1)
    where
      letter = letters !! fromIntegral (labelColumn l)

data PieceKind = Man | King
  deriving (Eq, Ord, Generic, Typeable)

instance Show PieceKind where
  show Man = "M"
  show King = "K"

-- | There are two places at the board for players: top and bottom.
data BoardSide = Top | Bottom
  deriving (Eq, Ord, Show, Generic, Typeable)

-- | Playing side. First is one who moves first.
-- Mapping of First\/Second to white\/black or to
-- top\/bottom depends on game rules.
-- Actually, we do not care at all about colors:
-- for example, in english draughts black are usually red;
-- but why should we care? it is only important that black
-- (well, red) move first.
data Side = First | Second
  deriving (Eq, Ord, Generic, Typeable)

instance Show Side where
  show First = "1"
  show Second = "2"

instance Store Side

-- | In most game rules, the side who moves first starts
-- from the bottom of board; but there are some (well,
-- english), in which first side starts from top.
data BoardOrientation = FirstAtBottom | SecondAtBottom
  deriving (Eq, Ord, Show, Generic, Typeable)

data Piece = Piece {
    pieceKind :: PieceKind
  , pieceSide :: Side
  }
  deriving (Eq, Ord, Typeable)

instance Show Piece where
  show (Piece k s) = show k ++ show s

type UnboxedPiece = Word8

data Address = Address {
    aLabel :: ! Label,
    aPromotionSide :: Maybe Side,
    aUpLeft :: Maybe Address,
    aUpRight :: Maybe Address,
    aDownLeft :: Maybe Address,
    aDownRight :: Maybe Address
  }
  deriving (Typeable)

instance Eq Address where
  f1 == f2 = aLabel f1 == aLabel f2

instance Show Address where
  show f = show (aLabel f)

instance Ord Address where  
  compare a1 a2 = compare (aLabel a1) (aLabel a2)

-- | Number of row / column of the board
type Line = Word8

type BoardSize = (Line, Line)

type FieldIndex = Int

type AddressMap a = IM.IntMap a

type LabelMap a = IM.IntMap a

type LabelSet = IS.IntSet

-- | Board describes current position on the board.
data Board = Board {
    bAddresses :: LabelMap Address,
    bCaptured :: LabelSet,
    boardCounts ::  BoardCounts,
    boardKey ::  ! BoardKey,
    bSize :: {-# UNPACK #-} ! BoardSize,
    boardHash :: {-# UNPACK #-} ! BoardHash,
    randomTable :: ! RandomTable
  }
  deriving (Typeable)

instance Eq Board where
  b1 == b2 = boardCounts b1 == boardCounts b2 && boardKey b1 == boardKey b2

-- | Statistic information about the board.
-- Can be used as a part of key in some caches.
data BoardCounts = BoardCounts {
    bcFirstMen :: ! Int
  , bcSecondMen :: ! Int
  , bcFirstKings :: ! Int
  , bcSecondKings :: ! Int
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Binary BoardCounts

instance Store BoardCounts

instance Hashable BoardCounts where
  hashWithSalt salt bc =
    salt `hashWithSalt` bcFirstMen bc `hashWithSalt` bcSecondMen bc `hashWithSalt` bcFirstKings bc `hashWithSalt` bcSecondKings bc

instance Hashable Board where
  hashWithSalt salt board = boardHash board

data BoardKey = BoardKey {
    bkFirstMen :: LabelSet
  , bkSecondMen :: LabelSet
  , bkFirstKings :: LabelSet
  , bkSecondKings :: LabelSet
  }
  deriving (Eq, Ord, Typeable, Generic)

instance Binary BoardKey

type BoardHash = Int
type RandomTable = UArray (UnboxedPiece, FieldIndex) BoardHash
type BoardData = UArray FieldIndex UnboxedPiece

class RandomTableProvider p where
  getRandomTable :: p -> RandomTable

type TBoardMap a = SM.Map Board a

-- | Direction on the board.
-- For example, B2 is at UpRight of A1.
data BoardDirection =
    UpLeft | UpRight 
  | DownLeft | DownRight
  deriving (Eq, Generic, Typeable)

instance Show BoardDirection where
  show UpLeft = "UL"
  show UpRight = "UR"
  show DownLeft = "DL"
  show DownRight = "DR"

-- | Direction from a point of view of a player.
-- For example, for white, B2 is at ForwardRight of A1;
-- for black, B2 is at BackwardLeft of A1.
data PlayerDirection =
    ForwardLeft | ForwardRight
  | BackwardLeft | BackwardRight
  deriving (Eq, Ord, Generic, Typeable)

instance Show PlayerDirection where
  show ForwardLeft = "FL"
  show ForwardRight = "FR"
  show BackwardLeft = "BL"
  show BackwardRight = "BR"

-- | One step of the move is a movement of piece
-- from one field to it's neighbour. At that moment
-- there can take place a capturing of another piece
-- or current piece promotion to king.
data Step = Step {
    sDirection :: ! PlayerDirection,
    sCapture :: ! Bool,
    sPromote :: ! Bool
  }
  deriving (Eq, Ord, Typeable)

instance Show Step where
  show step = show (sDirection step) ++ capture ++ promote
    where
      capture
        | sCapture step = "[X]"
        | otherwise = ""

      promote
        | sPromote step = "[K]"
        | otherwise = ""

-- | Move (or should we say half-move? because it's about one player's move) is
-- a series of steps from one field to neighbour, and to neighbour...
data Move = Move {
    moveBegin :: ! Address,
    moveSteps :: ! [Step]
  }
  deriving (Eq, Ord, Typeable)

instance Show Move where
  show move = "[" ++ show (moveBegin move) ++ "] " ++ (intercalate "." $ map show (moveSteps move))

-- | Representation of Step for JSON
data StepRep = StepRep {
    srField :: Label,
    srCapture :: Bool,
    srPromote :: Bool
  }
  deriving (Eq, Typeable, Generic)

instance Binary StepRep

instance Store StepRep

instance Show StepRep where
  show step = show (srField step) ++ capture ++ promote
    where
      capture
        | srCapture step = "[X]"
        | otherwise = ""

      promote
        | srPromote step = "[K]"
        | otherwise = ""

-- | Representation of Move for JSON.
data MoveRep =
    ShortMoveRep Label Label -- ^ Just start and end field specified
  | FullMoveRep Label [StepRep] -- ^ Full list of steps specified
  deriving (Eq, Typeable, Generic)

instance Binary MoveRep

instance Store MoveRep

instance Show MoveRep where
  show (ShortMoveRep from to) = show from ++ " > " ++ show to
  show (FullMoveRep from steps) = "[" ++ show from ++ "] " ++ (intercalate "." $ map show steps)

-- | Result of parsing MoveRep into Move
data MoveParseResult =
    Parsed Move
  | NoSuchMove
  | AmbigousMove [PossibleMove]
  deriving (Eq, Show)

data StepCheckResult =
    ValidStep Address
  | NoSuchNeighbour
  | NoPieceToCapture
  | CapturingOwnPiece
  | OccupatedField
  | InvalidPromotion Bool Bool
  deriving (Eq, Show)

data MoveCheckResult =
    ValidMove
  | InvalidStep Step StepCheckResult
  deriving (Eq, Show)

-- | Representation of Board for JSON
data BoardRep = BoardRep [(Label, Piece)]
  deriving (Eq, Ord, Show, Typeable)

-- | More convinient format for game rules to specify
-- which moves are possible
data PossibleMove = PossibleMove {
    pmBegin :: ! Address
  , pmEnd :: ! Address
  , pmVictims :: ! [Address] -- ^ list of captured fields
  , pmMove :: ! Move
  , pmPromote :: ! Bool      -- ^ is there any promotion in the move
  , pmResult :: ! [MoveAction]
  }
  deriving (Typeable)

instance Eq PossibleMove where
  pm1 == pm2 =
    pmBegin pm1 == pmBegin pm2 &&
    pmMove pm1 == pmMove pm2

instance Show PossibleMove where
  show pm = move ++ promotion
    where
      move
        | null (pmVictims pm) = show (pmBegin pm) ++ "-" ++ show (pmEnd pm)
        | otherwise =  show (pmBegin pm) ++ "x" ++ show (pmEnd pm)

      promotion
        | pmPromote pm = "(K)"
        | otherwise = ""

-- | The primitive action that can take place during the move
data MoveAction =
    Take ! Address            -- ^ Lift the piece from the board (at the beginning of the move)
  | RemoveCaptured ! Address  -- ^ Remove the piece that was captured (should be performed at the end of the move)
  | Put ! Address ! Piece       -- ^ Put the piece to the board (at the end of the move)
  deriving (Eq, Ord, Show, Typeable)

class HasBoardOrientation a where
  boardOrientation :: a -> BoardOrientation
  boardOrientation _ = FirstAtBottom

-- | Interface of game rules
class (Typeable g, Show g, HasBoardOrientation g) => GameRules g where
  -- | Initial board with initial pieces position
  initBoard :: SupervisorState -> g -> Board
  -- | Size of board used
  boardSize :: g -> BoardSize

  dfltEvaluator :: g -> SomeEval

  boardNotation :: g -> Label -> Notation
  parseNotation :: g -> Notation -> Either String Label

  possibleMoves :: g -> Side -> Board -> [PossibleMove]
  updateRules :: g -> Value -> g
  getGameResult :: g -> Board -> Maybe GameResult
  rulesName :: g -> String
  pdnId :: g -> String

fieldsCount :: GameRules rules => rules -> Line
fieldsCount rules =
  let (nrows, ncols) = boardSize rules
  in  nrows * ncols `div` 2

dfltBoardNotation :: Label -> Notation
dfltBoardNotation l = T.pack $ show l

data SomeRules = forall g. GameRules g => SomeRules g

instance Show SomeRules where
  show (SomeRules rules) = rulesName rules

type ScoreBase = Int16

data Score = Score {
      sNumeric :: ScoreBase
    , sPositional :: ScoreBase
    }
    deriving (Eq, Ord, Generic, Typeable, Bounded)

instance Store Score

instance Binary Score

scoreBound :: ScoreBase
scoreBound = 256

maxPieces :: ScoreBase
maxPieces = 30

win :: Score
win = Score maxPieces scoreBound

loose :: Score
loose = Score (-maxPieces) (-scoreBound)

clampS :: Int32 -> ScoreBase
clampS x = clampS' scoreBound x

clampS' :: ScoreBase -> Int32 -> ScoreBase
clampS' bound x = min bound $ max (-bound) (fromIntegral x)

safePlus :: forall a. (Integral a) => ScoreBase -> ScoreBase -> a -> ScoreBase
safePlus bound x y =
  let result = (fromIntegral x + fromIntegral y) :: Int32
  in  clampS' bound result

safeMinus :: forall a. (Integral a) => ScoreBase -> ScoreBase -> a -> ScoreBase
safeMinus bound x y =
  let result = (fromIntegral x - fromIntegral y) :: Int32
  in  clampS' bound result

safeScale :: forall a. (Integral a) => ScoreBase -> ScoreBase -> a -> ScoreBase
safeScale bound x y =
  let result = (fromIntegral x * fromIntegral y) :: Int32
  in  clampS' bound result

instance Num Score where
  fromInteger x = Score (fromIntegral x) 0
  (Score n1 p1) + (Score n2 p2) = Score (safePlus maxPieces n1 n2) (safePlus scoreBound p1 p2)
  (Score n1 p1) - (Score n2 p2) = Score (safeMinus maxPieces n1 n2) (safeMinus scoreBound p1 p2)
  _ * _ = error "* is not defined for Score"
  abs (Score n p) = Score (abs n) (abs p)
  negate (Score n p) = Score (negate n) (negate p)
  signum _ = error "signum is not defined for Score"

scaleScore :: Integral n => n -> Score -> Score
scaleScore x (Score n p) = Score (safeScale maxPieces (fromIntegral x) n)
                                 (safeScale scoreBound (fromIntegral x) p)

divideScore :: Integral n => Score -> n -> Score
divideScore (Score n p) d =
  Score (n `div` fromIntegral d) (p `div` fromIntegral d)

nextScore :: Score -> Score
nextScore (Score n p) = Score n (safePlus scoreBound p 1)

prevScore :: Score -> Score
prevScore (Score n p) = Score n (safeMinus scoreBound p 1)

instance Show Score where
  show (Score n p) = show n ++ "/" ++ show p

instance Formatable Score where
  formatVar _ (Score n p) = Right $ Builder.decimal n <> "/" <> Builder.decimal p

data GameResult =
    FirstWin
  | SecondWin
  | Draw
  deriving (Eq, Show, Ord, Typeable, Generic)

class (Show e, Typeable e) => Evaluator e where
  evalBoard :: e -> Side -> Board -> Score
  evaluatorName :: e -> String

  updateEval :: e -> Value -> e
  updateEval e _ = e

data SomeEval = forall e. Evaluator e => SomeEval e
  deriving (Typeable)

instance Show SomeEval where
  show (SomeEval e) = show e

instance Evaluator SomeEval where
  evalBoard (SomeEval e) s1 b = evalBoard e s1 b
  evaluatorName (SomeEval e) = evaluatorName e
  updateEval (SomeEval e) v = SomeEval (updateEval e v)

class (Show ai, Typeable (AiStorage ai)) => GameAi ai where
  type AiStorage ai

  createAiStorage :: ai -> Checkers (AiStorage ai)
  saveAiStorage :: ai -> AiStorage ai -> Checkers ()

  aiName :: ai -> String
  
  updateAi :: ai -> Value -> ai

  chooseMove :: ai -> AiStorage ai -> Side -> Board -> Checkers [PossibleMove]

  -- | Answer for a draw request.
  -- Default implementation always accepts the draw.
  decideDrawRequest :: ai -> AiStorage ai -> Side -> Board -> Checkers Bool
  decideDrawRequest _ _ _ _ = return True

data SomeAi = forall ai. GameAi ai => SomeAi ai

instance Show SomeAi where
  show (SomeAi ai) = show ai

updateSomeAi :: SomeAi -> Value -> SomeAi
updateSomeAi (SomeAi ai) params = SomeAi (updateAi ai params)

type GameId = String

data Player =
    User String
  | forall ai. GameAi ai => AI ai

instance Show Player where
  show (User name) = name
  show (AI ai) = aiName ai

isUser :: String -> Player -> Bool
isUser name (User n) = n == name
isUser _ _ = False

data GameStatus = New | Running | DrawRequested Side | Ended GameResult
  deriving (Eq, Show, Generic)

data Game = Game {
    getGameId :: GameId
  , gInitialBoard :: Board
  , gState :: GameState
  , gStatus :: GameStatus
  , gRules :: SomeRules
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: TChan Notify
  , gMsgbox2 :: TChan Notify
  }

instance Show Game where
  show g = printf "<Game: %s, 1: %s, 2: %s>"
                  (show $ gRules g)
                  (show $ gPlayer1 g)
                  (show $ gPlayer2 g)

instance Eq Game where
  g1 == g2 = getGameId g1 == getGameId g2

data GameState = GameState {
    gsSide :: Side
  , gsCurrentBoard :: Board
  , gsHistory :: [HistoryRecord]
  }

data HistoryRecord = HistoryRecord {
    hrSide :: Side
  , hrMove :: Move
  , hrPrevBoard :: Board
  }

data HistoryRecordRep = HistoryRecordRep {
    hrrSide :: Side
  , hrrMove :: MoveRep
  }
  deriving (Eq, Show, Typeable)

data Notify =
    MoveNotify {
      nDestination :: Side
    , nSource :: Side
    , nMove :: MoveRep
    , nBoard :: BoardRep
    }
  | UndoNotify {
      nDestination :: Side
    , nSource :: Side
    , nBoard :: BoardRep
    }
  | ResultNotify {
      nDestination :: Side
    , nSource :: Side
    , nResult :: GameResult
    }
  | DrawRqNotify {
      nDestination :: Side
    , nSource :: Side
    }
  | DrawRsNotify {
      nDestination :: Side
    , nSource :: Side
    , nAccepted :: Bool
    }
  | LogNotify {
      nDestination :: Side
    , nLevel :: String
    , nComponent :: String
    , nLogMessage :: TL.Text
    }
  deriving (Eq, Show, Generic)

-- | State of supervisor singleton
data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game                  -- ^ Set of games running
  , ssLastGameId :: Int                           -- ^ ID of last created game
  , ssAiStorages :: M.Map (String,String) Dynamic -- ^ AI storage instance per (AI engine; game rules) tuple
  , ssRandomTable :: RandomTable
  }

instance RandomTableProvider SupervisorState where
  getRandomTable = ssRandomTable

-- | Since many threads of REST server will refer
-- to supervisor's state, we have to put it into TVar
type SupervisorHandle = TVar SupervisorState

data AiConfig = AiConfig {
    aiThreads :: Int
  , aiLoadCache :: Bool
  , aiStoreCache :: Bool
  , aiUseCacheMaxDepth :: Int
  , aiUseCacheMaxPieces :: Int
  , aiUseCacheMaxDepthPlus :: Int
  , aiUseCacheMaxDepthMinus :: Int
  , aiUpdateCacheMaxDepth :: Int
  , aiUpdateCacheMaxPieces :: Int
  }
  deriving (Show, Typeable, Generic)

instance Default AiConfig where
  def = AiConfig {
          aiThreads = 4
        , aiLoadCache = True
        , aiStoreCache = False
        , aiUseCacheMaxDepth = 8
        , aiUseCacheMaxPieces = 24
        , aiUseCacheMaxDepthPlus = 0
        , aiUseCacheMaxDepthMinus = 0
        , aiUpdateCacheMaxDepth = 6
        , aiUpdateCacheMaxPieces = 8
      }

data GeneralConfig = GeneralConfig {
    gcHost :: T.Text
  , gcPort :: Int
  , gcEnableMetrics :: Bool
  , gcMetricsPort :: Int
  , gcLogFile :: FilePath
  , gcLogLevel :: Level
  , gcAiConfig :: AiConfig
  }
  deriving (Show, Typeable, Generic)

instance Default GeneralConfig where
  def = GeneralConfig {
    gcHost = "localhost",
    gcPort = 8864,
    gcEnableMetrics = True,
    gcMetricsPort = 8000,
    gcLogFile = "hcheckers.log",
    gcLogLevel = info_level,
    gcAiConfig = def
  }

-- | Commonly used data
data CheckersState = CheckersState {
    csLogging :: LoggingTState
  , csSupervisor :: SupervisorHandle
  , csMetrics :: Metrics.Metrics
  , csConfig :: GeneralConfig
  }

-- | Recognized exception types
data Error =
    NotYourTurn
  | NotAllowedMove
  | NoSuchMoveError
  | AmbigousMoveError [MoveRep]
  | NothingToUndo
  | NoSuchGame GameId
  | NoSuchUserInGame
  | UserNameAlreadyUsed
  | InvalidGameStatus GameStatus GameStatus -- ^ Expected, actual
  | TimeExhaused
  | InvalidBoard String
  | Unhandled String
  deriving (Eq, Show, Typeable, Generic)

instance Exception Error

-- | Checkers monad
newtype Checkers a = Checkers {
    runCheckers :: ExceptT Error (ReaderT CheckersState IO) a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader CheckersState, MonadError Error, MonadThrow, MonadCatch, MonadMask)

runCheckersT :: Checkers a -> CheckersState -> IO (Either Error a)
runCheckersT actions st = runReaderT (runExceptT $ runCheckers actions) st

forkCheckers :: Checkers () -> Checkers ()
forkCheckers actions = do
  st <- ask
  liftIO $ forkIO $ do
    res <- runCheckersT actions st
    case res of
      Right _ -> return ()
      Left err -> fail $ show err
  return ()

tryC :: Checkers a -> Checkers (Either Error a)
tryC actions =
  (do
   r <- actions
   return $ Right r) `catchError` (\e -> return $ Left e)

askSupervisor :: Checkers SupervisorHandle
askSupervisor = asks csSupervisor

askLogging :: Checkers LoggingTState
askLogging = asks csLogging

instance HasLogContext Checkers where
  getLogContext = asks (ltsContext . csLogging)

  withLogContext frame actions =
    Checkers $ ExceptT $ ReaderT $ \cs ->
      let logging = csLogging cs
          logging' = logging {ltsContext = frame : ltsContext logging} 
      in runReaderT (runExceptT $ runCheckers actions) $ cs {csLogging = logging'}

instance HasLogger Checkers where
  getLogger = asks (ltsLogger . csLogging)

  localLogger logger actions =
    Checkers $ ExceptT $ ReaderT $ \cs ->
      let logging = csLogging cs
          logging' = logging {ltsLogger = logger}
      in runReaderT (runExceptT $ runCheckers actions) $ cs {csLogging = logging'}

instance MonadMetrics Checkers where
  getMetrics = asks csMetrics

class HasMetricsConfig m where
  isMetricsEnabled :: m Bool

instance HasMetricsConfig Checkers where
  isMetricsEnabled = asks (gcEnableMetrics . csConfig)

timed :: String -> Checkers a -> Checkers a
timed message actions = do
    time1 <- liftIO $ getTime Realtime
    result <- actions
    time2 <- liftIO $ getTime Realtime
    let delta = time2 - time1
    $debug "{}: {}s + {}ns" (message, sec delta, nsec delta)
    return result

event :: (MonadIO m, MonadMask m) => String -> m a -> m a
event label actions =
  bracket_ (liftIO $ traceEventIO ("START " ++ label))
           (liftIO $ traceEventIO ("STOP " ++ label))
           actions

repeatTimed :: forall m. (MonadIO m, HasLogging m) => String -> Int -> m Bool -> m ()
repeatTimed label seconds action = repeatTimed' label seconds action' ()
  where

    action' _ = do
      continue <- action
      if continue
        then return ((), Just ())
        else return ((), Nothing)
  
repeatTimed' :: forall m a b. (MonadIO m, HasLogging m) => String -> Int -> (a -> m (b, Maybe a)) -> a -> m b
repeatTimed' label seconds action x = do
    start <- liftIO $ getTime Monotonic
    run 0 x start
  where
    run :: Int -> a -> TimeSpec -> m b
    run i x start = do
      (result, mbX') <- action x
      case mbX' of
        Just x' -> do
            time2 <- liftIO $ getTime Monotonic
            let delta = time2 - start
            if sec delta >= fromIntegral seconds
              then do
                  $info "{}: timeout exhaused, done {} iterations" (label, i+1)
                  return result
              else run (i+1) x' start
        Nothing -> do
            $info "{}: work done, in {} iterations" (label, i)
            return result

