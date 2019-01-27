{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Formats.Pdn where

import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Megaparsec hiding (Label, State)
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
pSemiMove rules = try full <|> try short
  where
    short = do
      from <- pLabel rules
      x <- oneOf ['-', 'x']
      let capture = (x == 'x')
      to <- pLabel rules
      return $ ShortSemiMoveRec from to capture

    full = do
      first <- pLabel rules
      char 'x'
      second <- pLabel rules
      char 'x'
      rest <- pLabel rules `sepBy1` char 'x'
      return $ FullSemiMoveRec (first : second : rest)

whitespace :: Parser ()
whitespace = label "white space or comment" $ do
  some $ try pComment <|> try pNag <|> space1
  return ()

pNag :: Parser ()
pNag = label "NAG" $ do
  char '$'
  some digitChar
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
  return $ MoveRec (Just first) second

pInstruction :: SomeRules -> Parser Instruction
pInstruction rules =
    (try pSetSecondMoveNr) <|> (try pSetMoveNr) <|>
    (try $ SemiMove `fmap` pSemiMove rules) <|> (try $ pVariant rules)

pSetMoveNr :: Parser Instruction
pSetMoveNr = do
  n <- some digitChar
  char '.'
  return $ SetMoveNumber (read n)

pSetSecondMoveNr :: Parser Instruction
pSetSecondMoveNr = do
  n <- some digitChar
  char '.'
  char '.'
  char '.'
  return $ SetSecondMoveNumber (read n)

pVariant :: SomeRules -> Parser Instruction
pVariant rules = between (char '(') (char ')') $ do
    optional whitespace
    instructions <- try (pInstruction rules) `sepEndBy` whitespace
    return $ Variant instructions

pResult :: Parser (Maybe GameResult)
pResult =
      (try $ string "*" >> return Nothing)
  <|> (try $ string "1-0" >> return (Just FirstWin))
  <|> (try $ string "2-0" >> return (Just FirstWin))
  <|> (try $ string "0-1" >> return (Just SecondWin))
  <|> (try $ string "0-2" >> return (Just SecondWin))
  <|> (try $ string "1/2-1/2" >> return (Just Draw))
  <|> (try $ string "1-1" >> return (Just Draw))

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
    pFenTag = do
      mbRules <- lift get
      case mbRules of
        Nothing -> fail "cant apply FEN: rules are not defined"
        Just rules -> pTag FEN "FEN" $ between (char '"') (char '"') (pFen rules)
    pGameTypeTag = do
      tag@(GameType rules) <- pTag GameType "GameType" $ between (char '"') (char '"') pGameType
      lift $ put $ Just rules
      return tag

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

pGame :: Maybe SomeRules -> Parser GameRecord
pGame dfltRules = do
  tags <- some (try pTag)
  case rulesFromTags tags `mplus` dfltRules of
    Nothing -> fail "Rules are not defined"
    Just rules -> do
      eol
      optional whitespace
      moves <- try (pInstruction rules) `sepEndBy` whitespace
      result <- pResult
      return $ GameRecord tags moves result

parsePdn :: Maybe SomeRules -> T.Text -> Either String GameRecord
parsePdn dfltRules text =
  case evalState (runParserT (pGame dfltRules) "<PDN>" text) dfltRules of
    Left err -> Left $ parseErrorPretty err
    Right pdn -> Right pdn

parsePdnFile :: Maybe SomeRules -> FilePath -> IO [GameRecord]
parsePdnFile dfltRules path = do
  text <- TIO.readFile path
  case evalState (runParserT ((pGame dfltRules) `sepEndBy` space1) path text) dfltRules of
    Left err -> fail $ parseErrorPretty err
    Right pdn -> return pdn

parseMoveRec :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Move
parseMoveRec rules side board rec =
  let moves = possibleMoves rules side board
      passedFields m = nonCaptureLabels rules side board (pmMove m) 
      suits m =
        case rec of
          ShortSemiMoveRec {..} ->
                aLabel (pmBegin m) == smrFrom &&
                aLabel (pmEnd m) == smrTo &&
                (not $ null $ pmVictims m) == smrCapture 
          FullSemiMoveRec {..} ->
                (not $ null $ pmVictims m) &&
                smrLabels `isSubsequenceOf` passedFields m
  in case filter suits moves of
    [m] -> pmMove m
    [] -> error $ printf "no such move: %s; side: %s; board: %s; possible: %s"
                    (show rec) (show side) (show board) (show $ map passedFields moves)
    ms -> error $ printf "ambigous move: %s; candidates are: %s; board: %s"
                    (show rec) (show ms) (show board)

fenFromTags :: [Tag] -> Maybe Fen
fenFromTags [] = Nothing
fenFromTags (FEN fen:_) = Just fen
fenFromTags (_:rest) = fenFromTags rest

initBoardFromTags :: SomeRules -> [Tag] -> Board
initBoardFromTags (SomeRules rules) tags =
  case fenFromTags tags of
    Nothing -> initBoard rules
    Just fen -> parseBoardRep rules $ fenToBoardRep fen

resultFromTags :: [Tag] -> Maybe GameResult
resultFromTags [] = Nothing
resultFromTags (Result result : _) = result
resultFromTags (_ : rest) = resultFromTags rest

data InterpreterState = InterpreterState {
    isCurrentVariant :: Int
  , isLastVariant :: Int
  , isCurrentMove :: Int
  , isCurrentSide :: Side
  , isVariants :: M.Map Int (M.Map Int MoveRec)
  }

type Interpreter a = State InterpreterState a

interpret :: Instruction -> Interpreter ()
interpret (SetMoveNumber n) =
  modify $ \st -> st {isCurrentMove = n, isCurrentSide = First}
interpret (SetSecondMoveNumber n) =
  modify $ \st -> st {isCurrentMove = n, isCurrentSide = Second}
interpret (SemiMove rec) = do
  side <- gets isCurrentSide
  variant <- gets isCurrentVariant
  moveNr <- gets isCurrentMove
  modify $ \st ->
    let updateVariant (Just moves) = Just $ M.alter setMove moveNr moves
        updateVariant Nothing = Just $ M.singleton moveNr singleMove

        singleMove :: MoveRec
        singleMove =
          case side of
            First -> MoveRec (Just rec) Nothing
            Second -> MoveRec Nothing (Just rec)

        setMove :: Maybe MoveRec -> Maybe MoveRec
        setMove Nothing = Just singleMove
        setMove (Just old) =
          case side of
            First -> Just $ old {mrFirst = Just rec}
            Second -> Just $ old {mrSecond = Just rec}

    in  st {isVariants = M.alter updateVariant variant (isVariants st)}
  when (side == First) $
    modify $ \st -> st {isCurrentSide = Second}
interpret (Variant instructions) = do
  src <- gets isCurrentVariant
  v <- gets isLastVariant
  init <- gets (fromJust . M.lookup src . isVariants)
  modify $ \st -> st {
      isLastVariant = v+1,
      isCurrentVariant = v+1,
      isVariants = M.insert (v+1) init (isVariants st)
    }

  forM_ instructions interpret

  modify $ \st -> st {isCurrentVariant = src}

instructionsToMoves :: [Instruction] -> [[MoveRec]]
instructionsToMoves instructions =
  let initState = InterpreterState 0 0 0 First M.empty
      state = execState (forM_ instructions interpret) initState
  in  map M.elems $ M.elems $ isVariants state

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
                  let board1 = case mrFirst moveRec of
                                 Just rec -> let move1 = parseMoveRec rules First board0 rec
                                                 (board1,_,_) = applyMove rules First move1 board0
                                                 in board1
                                 Nothing -> board0
                  in  case mrSecond moveRec of
                        Nothing -> board1
                        Just rec ->
                          let move2 = parseMoveRec rules Second board1 rec
                              (board2,_,_) = applyMove rules Second move2 board1
                          in  go board2 rest

            in  case instructionsToMoves (grMoves r) of
                  [moves] -> go board0 moves
                  vars -> error $ "multiple variants: " ++ show vars

    in  case findRules (grTags r) of
          Nothing -> error "rules are not specified"
          Just rules -> withRules rules

moveToInstructions :: Int -> MoveRec -> [Instruction]
moveToInstructions n move =
     [SetMoveNumber n]
      ++ case mrFirst move of
           Nothing -> []
           Just rec -> [SemiMove rec]
      ++ case mrSecond move of
           Nothing -> []
           Just rec -> [SemiMove rec]

movesToInstructions :: [MoveRec] -> [Instruction]
movesToInstructions moves = concat $ zipWith moveToInstructions [1..] moves

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

    moves = movesToInstructions $ translate (gRules game) board0 (reverse $ gsHistory $ gState game)

    board0 = case gRules game of
               SomeRules rules -> initBoard rules

    translate :: SomeRules -> Board -> [HistoryRecord] -> [MoveRec]
    translate _ _ [] = []
    translate rules board [r] = [MoveRec (Just $ translateMove rules First board $ hrMove r) Nothing]
    translate some@(SomeRules rules) board0 (r1:r2:rest) =
      let m1 = translateMove some First board0 $ hrMove r1
          (board1,_,_) = applyMove rules First (hrMove r1) board0
          m2 = translateMove some Second board1 $ hrMove r2
          (board2,_,_) = applyMove rules Second (hrMove r2) board1
          rec = MoveRec (Just m1) (Just m2)
      in  rec : translate some board2 rest

    translateMove :: SomeRules -> Side -> Board -> Move -> SemiMoveRec
    translateMove (SomeRules rules) side board move = 
      ShortSemiMoveRec {
          smrFrom = aLabel (moveBegin move)
        , smrTo = aLabel (moveEnd rules side board move)
        , smrCapture = isCaptureM move
        }

showPdn :: SomeRules -> GameRecord -> T.Text
showPdn (SomeRules rules) gr =
    T.unlines (map showTag $ grTags gr) <> "\n" <>
    T.unlines (zipWith showMove [1..] (head $ instructionsToMoves $ grMoves gr)) <> "\n" <>
    showResult (grResult gr)
  where
    showResult Nothing = "*"
    showResult (Just FirstWin) = "1-0"
    showResult (Just SecondWin) = "0-1"
    showResult (Just Draw) = "1/2-1/2"

    showMove n (MoveRec (Just s1) Nothing) = T.pack (show n) <> ". " <> showSemiMove s1
    showMove n (MoveRec (Just s1) (Just s2)) = T.pack (show n) <> ". " <> showSemiMove s1 <> " " <> showSemiMove s2

    showSemiMove (ShortSemiMoveRec from to False) =
      boardNotation rules from <> "-" <> boardNotation rules to
    showSemiMove (ShortSemiMoveRec from to True) =
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

