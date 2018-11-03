{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Formats.Fen where

import qualified Data.Text as T
import Data.Typeable
import Data.Char
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)
import Data.Void
import qualified Data.Text.IO as TIO

import Core.Types
import Core.Board
import Core.BoardMap
import Rules.International
import Formats.Types

pSide :: Parser Side
pSide = do
  l <- oneOf ['W', 'B'] :: Parser Char
  case l of
    'W' -> return First
    'B' -> return Second

pPiece :: SomeRules -> Parser (Label, PieceKind)
pPiece (SomeRules rules) = do
  kind <- do
          k <- optional $ try $ char 'K'
          case k of
            Nothing -> return Man
            Just _ -> return King
  not <- some digitChar
  lbl <- case parseNumericNotation (boardSize rules) (T.pack not) of
            Left err -> fail err
            Right lbl -> return lbl
  return (lbl, kind)

pFen :: SomeRules -> Parser Fen
pFen rules = do
    turn <- pSide
    char ':'
    side1 <- pSide
    pieces1 <- try (pPiece rules) `sepBy` char ','
    char ':'
    side2 <- pSide
    pieces2 <- try (pPiece rules) `sepBy` char ','
    if side1 == First
      then return $ Fen turn pieces1 pieces2
      else return $ Fen turn pieces2 pieces1

fenToBoardKey :: Fen -> BoardKey
fenToBoardKey fen =
  BoardKey {
    bkFirstMen = labelSetFromList [lbl | (lbl, Man) <- fenFirst fen],
    bkSecondMen = labelSetFromList [lbl | (lbl, Man) <- fenSecond fen],
    bkFirstKings = labelSetFromList [lbl | (lbl, King) <- fenFirst fen],
    bkSecondKings = labelSetFromList [lbl | (lbl, King) <- fenSecond fen]
  }

fenToBoardRep :: Fen -> BoardRep
fenToBoardRep fen =
  BoardRep $ [(lbl, Piece kind First) | (lbl, kind) <- fenFirst fen] ++
             [(lbl, Piece kind Second) | (lbl, kind) <- fenSecond fen]

parseFen :: SomeRules -> T.Text -> Either String BoardRep
parseFen rules text =
  case parse (pFen rules) "FEN" text of
    Left err -> Left $ parseErrorPretty err
    Right fen -> Right $ fenToBoardRep fen

