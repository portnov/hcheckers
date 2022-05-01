{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Formats.Compact where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec hiding (Label, State)
import Text.Megaparsec.Char
import qualified Data.Text.IO as TIO
import Text.Printf

import Core.Types
import Core.LabelSet as LS
import Core.Board
import Formats.Types
import Formats.Fen (boardToFen, showFen)
import Formats.Pdn (showPdn, movesToInstructions)
import Rules.Russian

data SemiMove =
    Skip
  | Short Line Line Line
  | Full Label Label
  deriving (Eq, Show)

pSemiMove :: Parser SemiMove
pSemiMove = try pSkip <|> try pShort <|> pFull

pSkip :: Parser SemiMove
pSkip = do
  string "---"
  return Skip

pLetter :: Parser Line
pLetter = do
  let letters = ['a' .. 'l']
  letter <- oneOf letters
  return $ fromIntegral $ ord letter - ord 'a'

pDigit :: Parser Line
pDigit = do
  ch <- digitChar
  return $ fromIntegral $ ord ch - ord '1'

pShort :: Parser SemiMove
pShort = do
  letter1 <- pLetter
  letter2 <- pLetter
  digit <- pDigit
  return $ Short letter1 letter2 digit

pFull :: Parser SemiMove
pFull = do
  letter1 <- pLetter
  digit1 <- pDigit
  char '-'
  letter2 <- pLetter
  digit2 <- pDigit
  return $ Full (mkLabel letter1 digit1) (mkLabel letter2 digit2)

pGame :: Parser [SemiMove]
pGame = try pSemiMove `sepBy` space1

pGames :: Parser [[SemiMove]]
pGames = try pGame `sepBy` char ';'

parseCompactFile :: FilePath -> IO [[SemiMove]]
parseCompactFile path = do
  text <- TIO.readFile path
  forM (T.lines text) $ \line -> do
    case evalState (runParserT pGame path line) Nothing of
      Left err -> fail $ errorBundlePretty err
      Right game -> return game

findMove :: Side -> SemiMove -> Board -> Either String PossibleMove
findMove side (Short colFrom colTo rowTo) board = 
  let suits pm = aLabel (pmEnd pm) == mkLabel colTo rowTo &&
                 labelColumn (aLabel (pmBegin pm)) == colFrom
  in case filter suits (possibleMoves russian side board) of
        [] -> Left $ printf "findMove: no pieces of %s at column %d" (show side) colFrom
        [pm] -> Right pm
        xs -> Left $ printf "findMove: ambigous move: %s" (show xs)

applySemiMove :: Side -> SemiMove -> Board -> Board
applySemiMove _ Skip b = b
applySemiMove _ (Full from to) board =
  let piece = fromJust $ getPiece' from board
      fromA = resolve from board
      toA   = resolve to board
      actions = [Take fromA, Put toA piece]
  in  applyMoveActions actions board
applySemiMove side sm@(Short colFrom colTo rowTo) board =
  case findMove side sm board of
    Left err -> error $ printf "applySemiMove: %s: %s" (show sm) err
    Right pm ->
      applyMoveActions (pmResult pm) board

convertSemiMove :: Side -> Board -> SemiMove -> Maybe SemiMoveRec
convertSemiMove _ _ Skip = Nothing
convertSemiMove _ _ (Full from to) = Just $ ShortSemiMoveRec from to False
convertSemiMove side board sm =
  case findMove side sm board of
    Left err -> error $ printf "convertSemiMove: %s: %s" (show sm) err
    Right pm -> Just $ ShortSemiMoveRec {
                         smrFrom = aLabel (pmBegin pm),
                         smrTo   = aLabel (pmEnd pm),
                         smrCapture = False
                       }

convertMoves :: SupervisorState -> [SemiMove] -> [MoveRec]
convertMoves rnd game = go (initBoard rnd russian) game
  where
    go _ [] = []
    go board0 [sm] =
      let smr = convertSemiMove First board0 sm
          board1 = applySemiMove First sm board0
          move = MoveRec smr Nothing
      in  [move]
    go board0 (sm1 : sm2 : rest) =
      let smr1 = convertSemiMove First board0 sm1
          board1 = applySemiMove First sm1 board0
          smr2 = convertSemiMove Second board1 sm2
          board2 = applySemiMove Second sm2 board1
          move = MoveRec smr1 smr2
      in  move : go board2 rest

gameToBoard :: SupervisorState -> [SemiMove] -> (Side, Board)
gameToBoard rnd game = go First (initBoard rnd russian) game
  where
    go side board [] = (side, board)
    go side board (sm : rest) =
      let board' = applySemiMove side sm board
      in  go (opposite side) board' rest

compactFileToFen :: SupervisorState -> FilePath -> IO ()
compactFileToFen rnd path = do
  games <- parseCompactFile path
  forM_ (zip [1.. ] games) $ \(i, game) -> do
    if null game
      then printf "empty game: %d" i
      else do
        let (side, board) = gameToBoard rnd game
            fen = boardToFen side board
            fenText = showFen (boardSize russian) fen
            targetPath = printf "draw%d.fen" (i :: Int)
        TIO.writeFile targetPath fenText

compactFileToPdn :: SupervisorState -> FilePath -> IO ()
compactFileToPdn rnd path = do
  games <- parseCompactFile path
  forM_ (zip [1.. ] $ filter (not . null) games) $ \(i, game) -> do
    let targetPath = printf "draw%d.pdn" (i :: Int)
        pdn = GameRecord tags (movesToInstructions moves) Nothing
        moves = convertMoves rnd game
        rules = SomeRules russian
        tags = [Event "Game Opening", GameType rules]
        pdnText = showPdn rules pdn
    TIO.writeFile targetPath pdnText

