{-# LANGUAGE DeriveGeneric #-}
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
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Dynamic
import Data.Aeson (Value)
import Data.Int
import Data.Word
import Data.Binary
import Data.Store
import Data.Hashable
import Text.Printf
import GHC.Generics
import System.Log.Heavy
import System.Log.Heavy.TH
import System.Clock

import Debug.Trace (traceEventIO)

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

data BoardSide = Top | Bottom
  deriving (Eq, Ord, Show, Generic, Typeable)

data Side = First | Second
  deriving (Eq, Ord, Generic, Typeable)

instance Show Side where
  show First = "1"
  show Second = "2"

instance Store Side

data BoardOrientation = FirstAtBottom | SecondAtBottom
  deriving (Eq, Ord, Show, Generic, Typeable)

data Piece = Piece PieceKind Side
  deriving (Eq, Ord, Typeable)

instance Show Piece where
  show (Piece k s) = show k ++ show s

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

type Line = Word8

type BoardSize = (Line, Line)

type FieldIndex = Int

type AddressMap a = IM.IntMap a

type LabelMap a = IM.IntMap a

type LabelSet = IS.IntSet

data Board = Board {
    bFirstMen :: LabelSet,
    bSecondMen :: LabelSet,
    bFirstKings :: LabelSet,
    bSecondKings :: LabelSet,
    bAddresses :: LabelMap Address,
    bCaptured :: LabelSet,
    boardCounts :: BoardCounts,
    boardKey :: BoardKey,
    bSize :: BoardSize
  }
  deriving (Typeable)

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

data BoardKey = BoardKey {
    bkFirstMen :: LabelSet
  , bkSecondMen :: LabelSet
  , bkFirstKings :: LabelSet
  , bkSecondKings :: LabelSet
  }
  deriving (Eq, Ord, Typeable, Generic)

instance Binary BoardKey

data BoardLineCounts = BoardLineCounts [Word8]
  deriving (Show)

instance Eq BoardLineCounts where
  (BoardLineCounts list1) == (BoardLineCounts list2) =
    let list1' = list1 ++ replicate (16 - length list1) 0
        list2' = list2 ++ replicate (16 - length list2) 0
    in  list1' == list2'

instance Data.Store.Store BoardLineCounts where
  size = ConstSize 16

  poke (BoardLineCounts list) =
    forM_ [0..15] $ \i ->
      if i > length list - 1
        then poke (0 :: Word8)
        else poke (fromIntegral (list !! i) :: Word8)

  peek = do
    bytes <- replicateM 16 (peek :: Peek Word8)
    return $ BoardLineCounts $ map fromIntegral bytes

type BoardMap a = M.Map BoardCounts (H.HashMap BoardKey a)

data BoardDirection =
    UpLeft | UpRight 
  | DownLeft | DownRight
  deriving (Eq, Generic, Typeable)

instance Show BoardDirection where
  show UpLeft = "UL"
  show UpRight = "UR"
  show DownLeft = "DL"
  show DownRight = "DR"

data PlayerDirection =
    ForwardLeft | ForwardRight
  | BackwardLeft | BackwardRight
  deriving (Eq, Ord, Generic, Typeable)

instance Show PlayerDirection where
  show ForwardLeft = "FL"
  show ForwardRight = "FR"
  show BackwardLeft = "BL"
  show BackwardRight = "BR"

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

data Move = Move {
    moveBegin :: ! Address,
    moveSteps :: ! [Step]
  }
  deriving (Eq, Ord, Typeable)

instance Show Move where
  show move = "[" ++ show (moveBegin move) ++ "] " ++ (intercalate "." $ map show (moveSteps move))

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

data MoveRep =
    ShortMoveRep Label Label
  | FullMoveRep Label [StepRep]
  deriving (Eq, Typeable, Generic)

instance Binary MoveRep

instance Store MoveRep

instance Show MoveRep where
  show (ShortMoveRep from to) = show from ++ " > " ++ show to
  show (FullMoveRep from steps) = "[" ++ show from ++ "] " ++ (intercalate "." $ map show steps)

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

data BoardRep = BoardRep [(Label, Piece)]
  deriving (Eq, Ord, Show, Typeable)

data PossibleMove = PossibleMove {
    pmBegin :: Address
  , pmEnd :: Address
  , pmVictims :: [Address]
  , pmMove :: Move
  , pmPromote :: Bool
  , pmResult :: [MoveAction]
  }
  deriving (Typeable)

instance Eq PossibleMove where
  pm1 == pm2 =
    pmBegin pm1 == pmBegin pm2 &&
    pmMove pm1 == pmMove pm2

instance Show PossibleMove where
  show pm = show (pmMove pm)

data MoveAction =
    Take Address
  | RemoveCaptured Address
  | Put Address Piece
  deriving (Eq, Ord, Show, Typeable)

class HasBoardOrientation a where
  boardOrientation :: a -> BoardOrientation
  boardOrientation _ = FirstAtBottom

class (Typeable g, Show g, Evaluator g, HasBoardOrientation g) => GameRules g where
  initBoard :: g -> Board
  boardSize :: g -> BoardSize

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

type Score = Int16

data GameResult =
    FirstWin
  | SecondWin
  | Draw
  deriving (Eq, Show, Ord, Typeable, Generic)

class Evaluator e where
  evalBoard :: e -> Side -> Side -> Board -> Score
  evaluatorName :: e -> String

class (Typeable ai, Show ai, Typeable (AiStorage ai)) => GameAi ai where
  type AiStorage ai

  createAiStorage :: ai -> Checkers (AiStorage ai)
  saveAiStorage :: ai -> AiStorage ai -> Checkers ()

  aiName :: ai -> String
  
  updateAi :: ai -> Value -> ai

  chooseMove :: ai -> AiStorage ai -> Side -> Board -> Checkers [Move]

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

data GameStatus = New | Running | Ended GameResult
  deriving (Eq, Show, Generic)

data Game = Game {
    getGameId :: GameId
  , gState :: GameState
  , gStatus :: GameStatus
  , gRules :: SomeRules
  , gPlayer1 :: Maybe Player
  , gPlayer2 :: Maybe Player
  , gMsgbox1 :: [Notify]
  , gMsgbox2 :: [Notify]
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
  deriving (Eq, Show, Generic)

data SupervisorState = SupervisorState {
    ssGames :: M.Map GameId Game
  , ssLastGameId :: Int
  , ssAiStorages :: M.Map (String,String) Dynamic
  }

type SupervisorHandle = TVar SupervisorState

data CheckersState = CheckersState {
    csLogging :: LoggingTState
  , csSupervisor :: SupervisorHandle
  , csMetrics :: Metrics.Metrics
  }

data Error =
    NotYourTurn
  | NotAllowedMove
  | NoSuchMoveError
  | AmbigousMoveError [MoveRep]
  | NothingToUndo
  | NoSuchGame GameId
  | NoSuchUserInGame
  | UserNameAlreadyUsed
  | Unhandled String
  deriving (Eq, Show, Typeable, Generic)

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

