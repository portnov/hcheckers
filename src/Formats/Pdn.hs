{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

module Formats.Pdn where

import Data.Char
import qualified Data.Text as T
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import Text.Megaparsec.Error (parseErrorPretty)
import qualified Data.Text.IO as TIO
import Text.Printf

import Core.Types
import Core.Board
import Rules.Diagonal
import Rules.English
import Rules.International
import Rules.Canadian
import Rules.Russian
import Rules.Simple
import Rules.Spancirety
import Formats.Types
import Formats.Fen

pLabel :: SomeRules -> Parser Label
pLabel (SomeRules rules) = do
  let letters = ['a' .. 'l']
  word <- some $ oneOf $ letters ++ map toUpper letters ++ ['0'..'9']
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
    pGameTypeTag = pTag GameType "GameType" $ between (char '"') (char '"') pGameType
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
    "20" -> return $ SomeRules international
    "21" -> return $ SomeRules english
    "25" -> return $ SomeRules russian
    "27" -> return $ SomeRules canadian
    "41" -> return $ SomeRules spancirety
    "42" -> return $ SomeRules diagonal
    "43" -> return $ SomeRules simple
    _ -> fail $ "Unsupported rules: " ++ n

rulesFromTags :: [Tag] -> Maybe SomeRules
rulesFromTags [] = Nothing
rulesFromTags (GameType r:_) = Just r
rulesFromTags (_:rest) = rulesFromTags rest

pGame :: Parser GameRecord
pGame = do
  tags <- some (try pTag)
  case rulesFromTags tags of
    Nothing -> fail "Rules are not defined"
    Just rules -> do
      eol
      moves <- try (pMove rules) `sepEndBy` whitespace
      result <- pResult
      return $ GameRecord tags moves result

parsePdn :: T.Text -> Either String GameRecord
parsePdn text =
  case parse pGame "<PDN>" text of
    Left err -> Left $ parseErrorPretty err
    Right pdn -> Right pdn

parsePdnFile :: FilePath -> IO [GameRecord]
parsePdnFile path = do
  text <- TIO.readFile path
  case parse (pGame `sepEndBy` space1) path text of
    Left err -> fail $ parseErrorPretty err
    Right pdn -> return pdn

parseMoveRec :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Move
parseMoveRec rules side board rec =
  let moves = possibleMoves rules side board
      suits m = aLabel (pmBegin m) == smrFrom rec &&
                aLabel (pmEnd m) == smrTo rec &&
                (not $ null $ pmVictims m) == smrCapture rec
  in case filter suits moves of
    [m] -> pmMove m
    [] -> error $ "no such move: " ++ show rec
    ms -> error $ "ambigous move: " ++ show ms

fenFromTags :: [Tag] -> Maybe Fen
fenFromTags [] = Nothing
fenFromTags (FEN fen:_) = Just fen
fenFromTags (_:rest) = fenFromTags rest

initBoardFromTags :: SomeRules -> [Tag] -> Board
initBoardFromTags (SomeRules rules) tags =
  case fenFromTags tags of
    Nothing -> initBoard rules
    Just fen -> parseBoardRep rules $ fenToBoardRep fen

loadPdn :: GameRecord -> Board
loadPdn r =
    let findRules [] = Nothing
        findRules (GameType rules:_) = Just rules
        findRules (_:rest) = findRules rest

        withRules :: SomeRules -> Board
        withRules some@(SomeRules rules) =
            let board0 = initBoardFromTags some (grTags r)
                
                go board [] = board
                go board0 (moveRec : rest) =
                  let move1 = parseMoveRec rules First board0 (mrFirst moveRec)
                      (board1,_,_) = applyMove rules First move1 board0
                  in  case mrSecond moveRec of
                        Nothing -> board1
                        Just rec ->
                          let move2 = parseMoveRec rules Second board1 rec
                              (board2,_,_) = applyMove rules Second move2 board1
                          in  go board2 rest

            in  go board0 (grMoves r)

    in  case findRules (grTags r) of
          Nothing -> error "rules are not specified"
          Just rules -> withRules rules

gameToPdn :: Game -> GameRecord
gameToPdn game =
    GameRecord {
      grTags = tags
    , grMoves = moves
    , grResult = result
    }
  where
    result = case gStatus game of
               Ended result -> Just result
               _ -> Nothing

    tags = [Event "HCheckers game", GameType (gRules game)]

    moves = translate (gRules game) board0 (reverse $ gsHistory $ gState game)

    board0 = case gRules game of
               SomeRules rules -> initBoard rules

    translate :: SomeRules -> Board -> [HistoryRecord] -> [MoveRec]
    translate _ _ [] = []
    translate rules board [r] = [MoveRec (translateMove rules First board $ hrMove r) Nothing]
    translate some@(SomeRules rules) board0 (r1:r2:rest) =
      let m1 = translateMove some First board0 $ hrMove r1
          (board1,_,_) = applyMove rules First (hrMove r1) board0
          m2 = translateMove some Second board1 $ hrMove r2
          (board2,_,_) = applyMove rules Second (hrMove r2) board1
          rec = MoveRec m1 (Just m2)
      in  rec : translate some board2 rest

    translateMove :: SomeRules -> Side -> Board -> Move -> SemiMoveRec
    translateMove (SomeRules rules) side board move = 
      SemiMoveRec {
          smrFrom = aLabel (moveBegin move)
        , smrTo = aLabel (moveEnd rules side board move)
        , smrCapture = isCapture move
        }

showPdn :: SomeRules -> GameRecord -> T.Text
showPdn (SomeRules rules) gr =
    T.unlines (map showTag $ grTags gr) <> "\n" <>
    T.unlines (zipWith showMove [1..] (grMoves gr)) <> "\n" <>
    showResult (grResult gr)
  where
    showResult Nothing = "*"
    showResult (Just FirstWin) = "1-0"
    showResult (Just SecondWin) = "0-1"
    showResult (Just Draw) = "1/2-1/2"

    showMove n (MoveRec s1 Nothing) = T.pack (show n) <> ". " <> showSemiMove s1
    showMove n (MoveRec s1 (Just s2)) = T.pack (show n) <> ". " <> showSemiMove s1 <> " " <> showSemiMove s2

    showSemiMove (SemiMoveRec from to False) =
      boardNotation rules from <> "-" <> boardNotation rules to
    showSemiMove (SemiMoveRec from to True) =
      boardNotation rules from <> "x" <> boardNotation rules to

    showTag (Event text) = T.pack (printf "[Event \"%s\"]" text)
    showTag (Site text) = T.pack (printf "[Site \"%s\"]" text)
    showTag (Date text) = T.pack (printf "[Date \"%s\"]" text)
    showTag (Round text) = T.pack (printf "[Round \"%s\"]" text)
    showTag (White text) = T.pack (printf "[White \"%s\"]" text)
    showTag (Black text) = T.pack (printf "[Black \"%s\"]" text)
    showTag (SetUp text) = T.pack (printf "[SetUp \"%s\"]" text)
    showTag (Opening text) = T.pack (printf "[Opening \"%s\"]" text)
    showTag (FEN fen) = T.pack (printf "[FEN \"%s\"]" (showFen (boardSize rules) fen))
    showTag (GameType _) = T.pack (printf "[GameType \"%s\"]" (pdnId rules))
    showTag (Unknown tag text) = T.pack (printf "[%s \"%s\"]" tag text)

