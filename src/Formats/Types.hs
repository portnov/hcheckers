{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}

module Formats.Types where

import Control.Monad.State
import qualified Data.Text as T
import Data.List (intercalate)
import Data.Typeable
import Text.Megaparsec hiding (Label, State)
import Data.Void
import Data.Aeson
import GHC.Generics

import Core.Types

type Parser a = ParsecT Void T.Text (State (Maybe SomeRules)) a

data Tag =
    Event T.Text
  | Site T.Text
  | Date T.Text
  | Round T.Text
  | White T.Text
  | Black T.Text
  | Result (Maybe GameResult)
  | SetUp T.Text
  | FEN Fen
  | GameType SomeRules
  | Opening T.Text
  | Unknown T.Text T.Text
  deriving (Show, Typeable)

data SemiMoveRec =
    FullSemiMoveRec {
      smrLabels :: [Label]
    }
  | ShortSemiMoveRec {
      smrFrom :: Label
    , smrTo :: Label
    , smrCapture :: Bool
    }
  deriving (Eq, Typeable)

instance Show SemiMoveRec where
  show (r@(ShortSemiMoveRec{}))
    | smrCapture r = show (smrFrom r) ++ "x" ++ show (smrTo r)
    | otherwise = show (smrFrom r) ++ "-" ++ show (smrTo r)
  show r = intercalate "x" $ map show (smrLabels r)

data MoveRec = MoveRec {
    mrFirst :: Maybe SemiMoveRec
  , mrSecond :: Maybe SemiMoveRec
  }
  deriving (Eq, Typeable)

instance Show MoveRec where
  show r = show (mrFirst r) ++ ", " ++ show (mrSecond r)

data Instruction =
      SetMoveNumber Int
    | SetSecondMoveNumber Int
    | SemiMove SemiMoveRec
    | Variant [Instruction]
  deriving (Typeable)

instance Show Instruction where
  show (SetMoveNumber n) = show n ++ "."
  show (SetSecondMoveNumber n) = show n ++ "..."
  show (SemiMove rec) = show rec
  show (Variant list) = show list

data GameRecord = GameRecord {
    grTags :: [Tag]
  , grMoves :: [Instruction]
  , grResult :: Maybe GameResult
  }
  deriving (Show, Typeable)

data PdnInfo = PdnInfo {
      pdnTitle :: Maybe T.Text
    , pdnRules :: Maybe String
    , pdnResult :: Maybe GameResult
    , pdnNextMove :: Side
  }
  deriving (Eq, Show, Typeable, Generic)

data Fen = Fen {
    fenNextMove :: Side
  , fenFirst :: [(Label, PieceKind)]
  , fenSecond :: [(Label, PieceKind)]
  }
  deriving (Eq, Show, Typeable)

