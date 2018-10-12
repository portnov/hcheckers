{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
module Types where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Aeson (Value)
import Text.Printf
import GHC.Generics

type Label = String

data PieceKind = Man | King
  deriving (Eq, Generic)

instance Show PieceKind where
  show Man = "M"
  show King = "K"

data Side = First | Second
  deriving (Eq, Generic)

instance Show Side where
  show First = "1"
  show Second = "2"

data Piece = Piece PieceKind Side
  deriving (Eq)

instance Show Piece where
  show (Piece k s) = show k ++ show s

data Address = Address {
    aLabel :: Label,
    aUpLeft :: Maybe Address,
    aUpRight :: Maybe Address,
    aDownLeft :: Maybe Address,
    aDownRight :: Maybe Address
  }

instance Eq Address where
  f1 == f2 = aLabel f1 == aLabel f2

instance Show Address where
  show f = aLabel f

instance Ord Address where  
  compare a1 a2 = compare (aLabel a1) (aLabel a2)

data Board = Board {
    bPieces :: M.Map Address Piece,
    bAddresses :: M.Map Label Address
  }

data BoardDirection =
    UpLeft | UpRight 
  | DownLeft | DownRight
  deriving (Eq, Generic)

instance Show BoardDirection where
  show UpLeft = "UL"
  show UpRight = "UR"
  show DownLeft = "DL"
  show DownRight = "DR"

data PlayerDirection =
    ForwardLeft | ForwardRight
  | BackwardLeft | BackwardRight
  deriving (Eq, Generic)

instance Show PlayerDirection where
  show ForwardLeft = "FL"
  show ForwardRight = "FR"
  show BackwardLeft = "BL"
  show BackwardRight = "BR"

data Step = Step {
    sDirection :: PlayerDirection,
    sCapture :: Bool,
    sPromote :: Bool
  }
  deriving (Eq)

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
    moveBegin :: Address,
    moveSteps :: [Step]
  }
  deriving (Eq)

instance Show Move where
  show move = "[" ++ show (moveBegin move) ++ "] " ++ (intercalate "." $ map show (moveSteps move))

data StepRep = StepRep {
    srField :: Label,
    srCapture :: Bool,
    srPromote :: Bool
  }
  deriving (Eq)

instance Show StepRep where
  show step = srField step ++ capture ++ promote
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
  deriving (Eq)

instance Show MoveRep where
  show (ShortMoveRep from to) = from ++ " > " ++ to
  show (FullMoveRep from steps) = "[" ++ from ++ "] " ++ (intercalate "." $ map show steps)

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
  deriving (Eq, Show)

class GameRules g where
  initBoard :: g -> Board
  possibleMoves :: g -> Side -> Board -> [Move]
  updateRules :: g -> Value -> g

data SomeRules = forall g. GameRules g => SomeRules g

class Evaluator e where
  evalBoard :: e -> Side -> Side -> Board -> Integer

class GameAi ai where
  chooseMove :: ai -> Side -> Board -> IO (Maybe Move)
  updateAi :: ai -> Value -> ai

data SomeAi = forall ai. GameAi ai => SomeAi ai

updateSomeAi :: SomeAi -> Value -> SomeAi
updateSomeAi (SomeAi ai) params = SomeAi (updateAi ai params)

