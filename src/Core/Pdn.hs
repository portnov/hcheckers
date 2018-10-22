{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Core.Pdn where

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

type Position = T.Text

data Tag =
    Event T.Text
  | Site T.Text
  | Date T.Text
  | Round T.Text
  | White T.Text
  | Black T.Text
  | Result GameResult
  | SetUp T.Text
  | FEN Position
  | GameType T.Text
  | Opening T.Text
  | Unknown T.Text T.Text
  deriving (Eq, Show, Typeable)

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
  , grResult :: GameResult
  }
  deriving (Eq, Show, Typeable)

pLabel :: Parser Label
pLabel = do
  letter <- oneOf letters
  digit <- oneOf ['1'..'8']
  let row = fromIntegral $ ord digit - ord '1'
      col = fromIntegral $ ord letter - ord 'a'
  return $ Label col row

pSemiMove :: Parser SemiMoveRec
pSemiMove = do
  from <- pLabel
  x <- oneOf ['-', 'x']
  let capture = (x == 'x')
  to <- pLabel
  return $ SemiMoveRec from to capture

whitespace :: Parser ()
whitespace = label "white space or comment" $ do
  some $ try pComment <|> space1
  return ()

pComment :: Parser ()
pComment = between (char '{') (char '}') $ do
  skipSome $ noneOf ['}']

pMove :: Parser MoveRec
pMove = do
  n <- some digitChar
  char '.'
  whitespace
  first <- pSemiMove
  whitespace
  second <- optional $ try pSemiMove
  return $ MoveRec first second

pResult :: Parser GameResult
pResult =
      (try $ string "*" >> return Ongoing)
  <|> (try $ string "1-0" >> return FirstWin)
  <|> (try $ string "0-1" >> return SecondWin)
  <|> (try $ string "1/2-1/2" >> return Draw)

pText :: Parser T.Text
pText = between (char '"') (char '"') $ do
  str <- many $ noneOf ['"']
  return $ T.pack str

pTag :: Parser Tag
pTag = do
    tag <- choice $ map try [pEvent, pSite, pDate, pRound, pWhite, pBlack, pResultTag, pFen, pGameType, pOpening, pUnknown]
    eol
    return tag
  where
    pTag tag name parser = between (char '[') (char ']') $ do
      string name
      space1
      value <- parser
      return $ tag value

    textTag tag name = pTag tag name pText

    pEvent = textTag Event "Event"
    pSite  = textTag Site "Site"
    pDate  = textTag Date "Date"
    pRound = textTag Round "Round"
    pWhite = textTag White "White"
    pBlack = textTag Black "Black"
    pResultTag = pTag Result "Result" $ between (char '"') (char '"') pResult
    pFen = textTag FEN "FEN"
    pGameType = textTag GameType "GameType"
    pOpening = textTag Opening "Opening"

    pUnknown = between (char '[') (char ']') $ do
      name <- some alphaNumChar
      space1
      value <- pText
      return $ Unknown (T.pack name) value

pGame :: Parser GameRecord
pGame = do
  tags <- some (try pTag)
  eol
  moves <- try pMove `sepEndBy` whitespace
  result <- pResult
  return $ GameRecord tags moves result

parsePdn :: FilePath -> IO [GameRecord]
parsePdn path = do
  text <- TIO.readFile path
  case parse (pGame `sepEndBy` space1) path text of
    Left err -> fail $ parseErrorPretty err
    Right pdn -> return pdn

