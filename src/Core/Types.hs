{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Core.Types where

import Control.Monad.Reader
import Control.Monad.Catch
import Control.Concurrent.STM
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.HashMap.Strict as H
import Data.Array
import Data.Dynamic
import Data.Aeson (Value)
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

data Side = First | Second
  deriving (Eq, Ord, Generic, Typeable)

instance Show Side where
  show First = "1"
  show Second = "2"

instance Store Side

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

type FieldIndex = (Line, Line)

type AddressMap a = Array FieldIndex (Maybe a)

type LabelMap a = Array FieldIndex a

data Board = Board {
    bPieces :: AddressMap Piece,
    bAddresses :: LabelMap Address,
    boardCounts :: BoardCounts,
    boardKey :: BoardKey,
    bSize :: BoardSize
  }
  deriving (Typeable)

instance Show Board where
  show board = show $ boardKey board

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
    bkFirstMen :: [Label]
  , bkSecondMen :: [Label]
  , bkFirstKings :: [Label]
  , bkSecondKings :: [Label]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Binary BoardKey

instance Store BoardKey where
  poke bk = do
    poke (fromIntegral (length (bkFirstMen bk)) :: Word8)
    forM_ (bkFirstMen bk) poke
    poke (fromIntegral (length (bkSecondMen bk)) :: Word8)
    forM_ (bkSecondMen bk) poke
    poke (fromIntegral (length (bkFirstKings bk)) :: Word8)
    forM_ (bkFirstKings bk) poke
    poke (fromIntegral (length (bkSecondKings bk)) :: Word8)
    forM_ (bkSecondKings bk) poke

  peek = do
    n <- peek :: Peek Word8
    firstMen <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    secondMen <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    firstKings <- replicateM (fromIntegral n) peek
    n <- peek :: Peek Word8
    secondKings <- replicateM (fromIntegral n) peek
    return $ BoardKey firstMen secondMen firstKings secondKings

  size = VarSize $ \bk ->
         4 + length (bkFirstMen bk) +
             length (bkSecondMen bk) +
             length (bkFirstKings bk)  +
             length (bkSecondKings bk) 
  

instance Hashable BoardKey where
  hashWithSalt salt bk =
    salt `hashWithSalt` bkFirstMen bk `hashWithSalt` bkSecondMen bk `hashWithSalt` bkFirstKings bk `hashWithSalt` bkSecondKings bk

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
  | AmbigousMove [Move]
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

class (Ord g, Typeable g, Show g) => GameRules g where
  initBoard :: g -> Board
  boardSize :: g -> BoardSize
  boardNotation :: g -> Label -> Notation
  parseNotation :: g -> Notation -> Either String Label
  possibleMoves :: g -> Side -> Board -> [Move]
  updateRules :: g -> Value -> g
  getGameResult :: g -> Board -> GameResult
  rulesName :: g -> String

dfltBoardNotation :: Label -> Notation
dfltBoardNotation l = T.pack $ show l

data SomeRules = forall g. GameRules g => SomeRules g

instance Show SomeRules where
  show (SomeRules rules) = rulesName rules

type Score = Int

data GameResult =
    FirstWin
  | SecondWin
  | Draw
  | Ongoing
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

data GameStatus = New | Running
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
  }

newtype Checkers a = Checkers {
    runCheckers :: ReaderT CheckersState IO a
  }
  deriving (Applicative, Functor, Monad, MonadIO, MonadReader CheckersState, MonadThrow, MonadCatch, MonadMask)

runCheckersT :: Checkers a -> CheckersState -> IO a
runCheckersT actions st = runReaderT (runCheckers actions) st

askSupervisor :: Checkers SupervisorHandle
askSupervisor = asks csSupervisor

askLogging :: Checkers LoggingTState
askLogging = asks csLogging

instance HasLogContext Checkers where
  getLogContext = asks (ltsContext . csLogging)

  withLogContext frame actions =
    Checkers $ ReaderT $ \cs ->
      let logging = csLogging cs
          logging' = logging {ltsContext = frame : ltsContext logging} 
      in runReaderT (runCheckers actions) $ cs {csLogging = logging'}

instance HasLogger Checkers where
  getLogger = asks (ltsLogger . csLogging)

  localLogger logger actions =
    Checkers $ ReaderT $ \cs ->
      let logging = csLogging cs
          logging' = logging {ltsLogger = logger}
      in runReaderT (runCheckers actions) $ cs {csLogging = logging'}

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

