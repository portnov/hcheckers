{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Formats.Fen where

import Control.Monad.State
import qualified Data.Text as T
import Data.Monoid ((<>))
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)

import Core.Types
import Core.Board
import Core.BoardMap
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
  case evalState (runParserT (pFen rules) "FEN" text) Nothing of
    Left err -> Left $ parseErrorPretty err
    Right fen -> Right $ fenToBoardRep fen

boardToFen :: Side -> Board -> Fen
boardToFen side b =
  Fen {
    fenNextMove = side,
    fenFirst = [(lbl, Man) | lbl <- labelSetToList (bFirstMen b)] ++
               [(lbl, King) | lbl <- labelSetToList (bFirstKings b)],
    fenSecond = [(lbl, Man) | lbl <- labelSetToList (bSecondMen b)] ++
                [(lbl, King) | lbl <- labelSetToList (bSecondKings b)]
  }

showFen :: BoardSize -> Fen -> T.Text
showFen bsize fen =
    showSide (fenNextMove fen) <> ":W" <>
    T.intercalate "," (map showPiece $ fenFirst fen) <> ":B" <>
    T.intercalate "," (map showPiece $ fenSecond fen)
  where
    showSide First = "W"
    showSide Second = "B"

    showPiece (lbl, Man) = numericNotation bsize lbl
    showPiece (lbl, King) = "K" <> numericNotation bsize lbl

