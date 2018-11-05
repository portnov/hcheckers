{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Formats.Pdn where

import qualified Data.Text as T
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)
import qualified Data.Text.IO as TIO

import Core.Types
import Rules.Diagonal
import Rules.English
import Rules.International
import Rules.Russian
import Rules.Simple
import Rules.Spancirety
import Formats.Types
import Formats.Fen

pLabel :: SomeRules -> Parser Label
pLabel (SomeRules rules) = do
  word <- some $ oneOf $ letters ++ ['0'..'9']
  case parseNotation rules (T.pack word) of
    Left err -> fail err
    Right label -> return label

pSemiMove :: SomeRules -> Parser SemiMoveRec
pSemiMove rules = do
  from <- pLabel rules
  x <- oneOf ['-', 'x']
  let capture = (x == 'x')
  to <- pLabel rules
  return $ SemiMoveRec from to capture

whitespace :: Parser ()
whitespace = label "white space or comment" $ do
  some $ try pComment <|> space1
  return ()

pComment :: Parser ()
pComment = between (char '{') (char '}') $ do
  skipSome $ noneOf ['}']

pMove :: SomeRules -> Parser MoveRec
pMove rules = do
  n <- some digitChar
  char '.'
  whitespace
  first <- pSemiMove rules
  whitespace
  second <- optional $ try (pSemiMove rules)
  return $ MoveRec first second

pResult :: Parser (Maybe GameResult)
pResult =
      (try $ string "*" >> return Nothing)
  <|> (try $ string "1-0" >> return (Just FirstWin))
  <|> (try $ string "0-1" >> return (Just SecondWin))
  <|> (try $ string "1/2-1/2" >> return (Just Draw))

pText :: Parser T.Text
pText = between (char '"') (char '"') $ do
  str <- many $ noneOf ['"']
  return $ T.pack str

pTag :: Parser Tag
pTag = do
    tag <- choice $ map try [pEvent, pSite, pDate, pRound, pWhite, pBlack, pResultTag, pFenTag, pGameTypeTag, pOpening, pUnknown]
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
    pFenTag = pTag FEN "FEN" (pFen (SomeRules international))
    pGameTypeTag = pTag GameType "GameType" pGameType
    pOpening = textTag Opening "Opening"

    pUnknown = between (char '[') (char ']') $ do
      name <- some alphaNumChar
      space1
      value <- pText
      return $ Unknown (T.pack name) value

pGameType :: Parser SomeRules
pGameType = do
  n <- some digitChar
  case n of
    "42" -> return $ SomeRules diagonal
    "21" -> return $ SomeRules english
    "20" -> return $ SomeRules international
    "25" -> return $ SomeRules russian
    "43" -> return $ SomeRules simple
    "41" -> return $ SomeRules spancirety
    _ -> fail $ "Unsupported rules: " ++ n

selectRules :: [Tag] -> Parser SomeRules
selectRules [] = fail "Rules are not defined"
selectRules (GameType r:_) = return r
selectRules (_:rest) = selectRules rest

pGame :: Parser GameRecord
pGame = do
  tags <- some (try pTag)
  rules <- selectRules tags
  eol
  moves <- try (pMove rules) `sepEndBy` whitespace
  result <- pResult
  return $ GameRecord tags moves result

parsePdn :: FilePath -> IO [GameRecord]
parsePdn path = do
  text <- TIO.readFile path
  case parse (pGame `sepEndBy` space1) path text of
    Left err -> fail $ parseErrorPretty err
    Right pdn -> return pdn

