{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
module Types where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Array
import Data.String
import Data.Char (isDigit, ord)
import Data.Aeson (Value)
import Data.Typeable
import Data.Int
import Data.Word
import Data.Binary
import Data.Store
import Text.Printf
import GHC.Generics

data Label = Label {
    labelColumn :: ! Line,
    labelRow :: ! Line
  }
  deriving (Eq, Ord, Typeable, Generic)

instance Binary Label where

instance Store Label

letters :: [Char]
letters = "abcdefgh" 

instance Show Label where
  show l = letter : show (labelRow l + 1)
    where
      letter = letters !! fromIntegral (labelColumn l)

instance IsString Label where
  fromString [l,d]
    | isDigit d = case elemIndex l letters of
                    Nothing -> error $ "Label.fromString: unknown letter: " ++ [l]
                    Just col -> let row = ord d - ord '1'
                                in  Label (fromIntegral col) (fromIntegral row)
  fromString e = error $ "Label.fromString: cant parse: " ++ e
    
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

type Line = Int

type FieldIndex = (Line, Line)

type AddressMap a = Array FieldIndex (Maybe a)

type LabelMap a = Array FieldIndex a

data Board = Board {
    bPieces :: AddressMap Piece,
    bAddresses :: LabelMap Address,
    boardCounts :: BoardCounts,
    boardKey :: BoardKey
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

data BoardKey = BoardKey {
    bkFirstMen :: [Label]
  , bkSecondMen :: [Label]
  , bkFirstKings :: [Label]
  , bkSecondKings :: [Label]
  }
  deriving (Eq, Ord, Show, Typeable, Generic)

instance Binary BoardKey

instance Store BoardKey

type BoardMap a = M.Map BoardCounts (M.Map BoardKey a)

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
  deriving (Eq, Generic, Typeable)

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
  deriving (Eq, Typeable)

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
  deriving (Eq, Typeable)

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

class Typeable g => GameRules g where
  initBoard :: g -> Board
  possibleMoves :: g -> Side -> Board -> [Move]
  updateRules :: g -> Value -> g
  rulesName :: g -> String

data SomeRules = forall g. GameRules g => SomeRules g

type Score = Int

class Evaluator e where
  evalBoard :: e -> Side -> Side -> Board -> Score
  evaluatorName :: e -> String

class (Typeable ai, Typeable (AiStorage ai)) => GameAi ai where
  type AiStorage ai

  createAiStorage :: ai -> IO (AiStorage ai)
  saveAiStorage :: ai -> AiStorage ai -> IO ()

  aiName :: ai -> String
  
  updateAi :: ai -> Value -> ai

  chooseMove :: ai -> AiStorage ai -> Side -> Board -> IO [Move]

data SomeAi = forall ai. GameAi ai => SomeAi ai

updateSomeAi :: SomeAi -> Value -> SomeAi
updateSomeAi (SomeAi ai) params = SomeAi (updateAi ai params)

