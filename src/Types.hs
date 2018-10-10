module Types where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.Printf

data PieceKind = Man | King
  deriving (Eq)

instance Show PieceKind where
  show Man = "M"
  show King = "K"

data Side = First | Second
  deriving (Eq)

instance Show Side where
  show First = "1"
  show Second = "2"

data Piece = Piece PieceKind Side
  deriving (Eq)

instance Show Piece where
  show (Piece k s) = show k ++ show s

data Address = Address {
    aLabel :: String,
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
    bAddresses :: M.Map String Address
  }

data BoardDirection =
    UpLeft | UpRight 
  | DownLeft | DownRight
  deriving (Eq)

instance Show BoardDirection where
  show UpLeft = "UL"
  show UpRight = "UR"
  show DownLeft = "DL"
  show DownRight = "DR"

data PlayerDirection =
    ForwardLeft | ForwardRight
  | BackwardLeft | BackwardRight
  deriving (Eq)

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

class GameRules g where
  possibleMoves :: g -> Side -> Board -> [Move]
  -- applyMove :: g -> Side -> Move -> Board -> Board

class Evaluator e where
  evalBoard :: e -> Side -> Board -> Integer

class GameAi ai where
  chooseMove :: ai -> Side -> Board -> IO Move

