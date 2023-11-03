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
import Rules.Frisian
import Rules.Killer

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
    "40" -> return $ SomeRules frisian
    "41" -> return $ SomeRules spancirety
    "42" -> return $ SomeRules diagonal
    "43" -> return $ SomeRules simple
    "44" -> return $ SomeRules killer
    _ -> fail $ "Unsupported rules: " ++ n

titleFromTags :: [Tag] -> Maybe T.Text
titleFromTags [] = Nothing
titleFromTags (Event title:_) = Just title
titleFromTags (_: rest) = titleFromTags rest

rulesFromTags :: [Tag] -> Maybe SomeRules
rulesFromTags [] = Nothing
rulesFromTags (GameType r:_) = Just r
rulesFromTags (_:rest) = rulesFromTags rest

rulesFromPdn :: GameRecord -> Maybe SomeRules
rulesFromPdn gr = rulesFromTags (grTags gr)

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

parseSemiMove :: SomeRules -> T.Text -> Either String SemiMoveRec
parseSemiMove rules text =
  case evalState (runParserT (pSemiMove rules) "<MOVE>" text) (Just rules) of
    Left err -> Left $ errorBundlePretty err
    Right rec -> Right rec

parseSemiMove' :: SomeRules -> Side -> Board -> T.Text -> Either Error PossibleMove
parseSemiMove' rules@(SomeRules g) side board text =
  case parseSemiMove rules text of
    Left err -> Left $ Unhandled err
    Right rec -> unambigousMove (T.unpack text) g side board (parseMoveRec' g side board rec)

parsePdn :: Maybe SomeRules -> T.Text -> Either String GameRecord
parsePdn dfltRules text =
  case evalState (runParserT (pGame dfltRules) "<PDN>" text) dfltRules of
    Left err -> Left $ errorBundlePretty err
    Right pdn -> Right pdn

parsePdnFile :: Maybe SomeRules -> FilePath -> IO [GameRecord]
parsePdnFile dfltRules path = do
  text <- TIO.readFile path
  case evalState (runParserT ((pGame dfltRules) `sepEndBy` space1) path text) dfltRules of
    Left err -> fail $ errorBundlePretty err
    Right pdn -> return pdn

parseMoveRec' :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> [PossibleMove]
parseMoveRec' rules side board rec =
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
  in filter suits moves

unambigousMove :: GameRules rules => String -> rules -> Side -> Board -> [PossibleMove] -> Either Error PossibleMove
unambigousMove msg rules side board moves =
  case moves of
    [m] -> return m
    [] -> Left $ NoSuchMoveExt msg side (boardRep board) (map (moveRep rules side . pmMove) moves)
    ms -> Left $ AmbigousPdnMove msg (show ms) (boardRep board)

parseMoveRec :: GameRules rules => rules -> Side -> Board -> SemiMoveRec -> Either Error Move
parseMoveRec rules side board rec =
  case unambigousMove (show rec) rules side board (parseMoveRec' rules side board rec) of
    Right pm -> Right (pmMove pm)
    Left err -> Left err

fenFromTags :: [Tag] -> Maybe Fen
fenFromTags [] = Nothing
fenFromTags (FEN fen:_) = Just fen
fenFromTags (_:rest) = fenFromTags rest

initBoardFromTags :: SupervisorState -> SomeRules -> [Tag] -> Board
initBoardFromTags rnd (SomeRules rules) tags =
  case fenFromTags tags of
    Nothing -> initBoard rnd rules
    Just fen -> parseBoardRep rnd rules $ fenToBoardRep fen

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

loadPdn :: SupervisorState -> GameRecord -> Either Error ([HistoryRecord], Board)
loadPdn rnd r = do
    let findRules [] = Nothing
        findRules (GameType rules:_) = Just rules
        findRules (_:rest) = findRules rest

        withRules :: SomeRules -> Either Error ([HistoryRecord], Board)
        withRules some@(SomeRules rules) = do
            let board0 = initBoardFromTags rnd some (grTags r)
                
                go :: ([HistoryRecord], Board) -> [MoveRec] -> Either Error ([HistoryRecord], Board)
                go (history, board) [] = return (history, board)
                go (history, board0) (moveRec : rest) = do
                  (records1, board1) <-
                      case mrFirst moveRec of
                        Just rec -> do
                          move1 <- parseMoveRec rules First board0 rec
                          let (board1,_,_) = applyMove rules First move1 board0
                              record = HistoryRecord First move1 board0
                          return ([record], board1)
                        Nothing -> return ([], board0)
                  case mrSecond moveRec of
                    Nothing -> return (records1 ++ history, board1)
                    Just rec -> do
                      move2 <- parseMoveRec rules Second board1 rec
                      let (board2,_,_) = applyMove rules Second move2 board1
                          record = HistoryRecord Second move2 board1
                      go ([record] ++ records1 ++ history, board2) rest

            case instructionsToMoves (grMoves r) of
              [moves] -> go ([], board0) moves
              vars -> Left $ AmbigousPdnInstruction $ show vars

    case findRules (grTags r) of
      Nothing -> Left $ Unhandled "rules are not specified"
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

captureLabels :: MoveRep -> [Label]
captureLabels (FullMoveRep from steps) =
  from : (map srField $ filter (not . srCapture) steps)

gameToPdn :: SupervisorState -> Game -> GameRecord
gameToPdn rnd game =
    GameRecord {
      grTags = tags
    , grMoves = moves
    , grResult = result
    }
  where
    result = case gStatus game of
               Ended result -> Just result
               _ -> Nothing

    tags = [Event "HCheckers game", GameType (gRules game), FEN (boardToFen First $ gInitialBoard game)]
            ++ [firstPlayerTag] ++ [secondPlayerTag]

    invert = boardOrientation (gRules game) == SecondAtBottom
    firstName  = getPlayerName game First
    secondName = getPlayerName game Second

    (firstPlayerTag, secondPlayerTag)
      | invert    = (Black firstName, White secondName)
      | otherwise = (White firstName, Black secondName)

    moves = movesToInstructions $ translate (gRules game) board0 (reverse $ gsHistory $ gState game)

    board0 = case gRules game of
               SomeRules rules -> initBoard rnd rules

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
    translateMove (SomeRules rules) side board move
      | isCaptureM move =
          let rep = moveRep rules side move
          in  FullSemiMoveRec $ captureLabels rep
      | otherwise =
          ShortSemiMoveRec {
              smrFrom = aLabel (moveBegin move)
            , smrTo = aLabel (moveEnd rules side board move)
            , smrCapture = False
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
    showSemiMove (FullSemiMoveRec labels) =
      T.intercalate "x" $ map (boardNotation rules) labels

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

pdnInfo :: GameRecord -> PdnInfo
pdnInfo gr = PdnInfo {
        pdnTitle = titleFromTags (grTags gr)
      , pdnRules = someRulesName (rulesFromTags $ grTags gr)
      , pdnResult = grResult gr
      , pdnNextMove = getNextMove (last $ head $ instructionsToMoves $ grMoves gr)
    }
  where
    someRulesName Nothing = Nothing
    someRulesName (Just (SomeRules rules)) = Just (rulesName rules)

    getNextMove r =
      case mrSecond r of
        Nothing -> Second
        _ -> First

