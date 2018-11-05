{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Formats.Types where

import qualified Data.Text as T
import Data.Typeable
import Data.Char
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)
import Data.Void
import qualified Data.Text.IO as TIO

import Core.Types

type Parser a = Parsec Void T.Text a

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

data SemiMoveRec = SemiMoveRec {
    smrFrom :: Label
  , smrTo :: Label
  , smrCapture :: Bool
  }
  deriving (Eq, Show, Typeable)

data MoveRec = MoveRec {
    mrFirst :: SemiMoveRec
  , mrSecond :: Maybe SemiMoveRec
  }
  deriving (Eq, Show, Typeable)

data GameRecord = GameRecord {
    grTags :: [Tag]
  , grMoves :: [MoveRec]
  , grResult :: Maybe GameResult
  }
  deriving (Show, Typeable)

data Fen = Fen {
    fenNextMove :: Side
  , fenFirst :: [(Label, PieceKind)]
  , fenSecond :: [(Label, PieceKind)]
  }
  deriving (Eq, Show, Typeable)

