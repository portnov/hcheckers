{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Core.Board where

import Control.Monad
import Data.Maybe
import Data.List
import Data.String
import Data.Char (isDigit, toLower, toUpper)
import qualified Data.Map as M
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import Data.Array.IArray as A
import Data.Bits (xor)
import Text.Printf

-- import Debug.Trace

import Core.Types
import Core.BoardMap

opposite :: Side -> Side
opposite First = Second
opposite Second = First

isMan :: Piece -> Bool
isMan (Piece kind _) = kind == Man

isKing :: Piece -> Bool
isKing (Piece kind _) = kind == King

promotePiece :: Piece -> Piece
promotePiece (Piece Man side) = Piece King side
promotePiece p = p

promoteK :: PieceKind -> PieceKind
promoteK Man = King
promoteK King = King

opponentPiece :: Piece -> Piece
opponentPiece (Piece k s) = Piece k (opposite s)

allFields :: Board -> [FieldIndex]
allFields b = [mkIndex col row | col <- [0..ncols-1], row <- [0..nrows-1]]
  where
    (ncols, nrows) = bSize b

allLabels :: Board -> [Label]
allLabels b = [Label col row | col <- [0..ncols-1], row <- [0..nrows-1]]
  where
    (ncols, nrows) = bSize b

allPieces :: Board -> [Maybe Piece]
allPieces b =
    [getPiece' (Label col row) b | col <- [0 .. ncols-1], row <-  [0 .. nrows-1]]
  where
    (ncols, nrows) = bSize b

boardDirection :: BoardSide -> PlayerDirection -> BoardDirection
boardDirection Bottom ForwardLeft = UpLeft
boardDirection Bottom ForwardRight = UpRight
boardDirection Bottom BackwardLeft = DownLeft
boardDirection Bottom BackwardRight = DownRight
boardDirection Bottom Forward = Up
boardDirection Bottom Backward = Down
boardDirection Top ForwardLeft = DownRight
boardDirection Top ForwardRight = DownLeft
boardDirection Top BackwardLeft = UpRight
boardDirection Top BackwardRight = UpLeft
boardDirection Top Backward = Up
boardDirection Top Forward = Down
boardDirection _ PRight = ToRight
boardDirection _ PLeft = ToLeft

boardSide :: BoardOrientation -> Side -> BoardSide
boardSide FirstAtBottom First = Bottom
boardSide FirstAtBottom Second = Top
boardSide SecondAtBottom First = Top
boardSide SecondAtBottom Second = Bottom

playerSide :: BoardOrientation -> BoardSide -> Side
playerSide FirstAtBottom Bottom = First
playerSide FirstAtBottom Top = Second
playerSide SecondAtBottom Bottom = Second
playerSide SecondAtBottom Top = First

myDirection :: HasBoardOrientation rules => rules -> Side -> PlayerDirection -> BoardDirection
myDirection rules side dir = boardDirection (boardSide (boardOrientation rules) side) dir

playerDirection :: Side -> BoardDirection -> PlayerDirection
playerDirection First UpLeft = ForwardLeft
playerDirection First UpRight = ForwardRight
playerDirection First DownLeft = BackwardLeft
playerDirection First DownRight = BackwardRight
playerDirection First Up = Forward
playerDirection First Down = Backward
playerDirection Second UpLeft = BackwardRight
playerDirection Second UpRight = BackwardLeft
playerDirection Second DownLeft = ForwardRight
playerDirection Second DownRight = ForwardLeft
playerDirection Second Down = Forward
playerDirection Second Up = Backward
playerDirection _ ToRight = PRight
playerDirection _ ToLeft = PLeft

oppositeDirection :: PlayerDirection -> PlayerDirection
oppositeDirection ForwardLeft = BackwardRight
oppositeDirection ForwardRight = BackwardLeft
oppositeDirection BackwardLeft = ForwardRight
oppositeDirection BackwardRight = ForwardLeft
oppositeDirection Forward = Backward
oppositeDirection Backward = Forward
oppositeDirection PRight = PLeft
oppositeDirection PLeft = PRight

neighbour :: HasBoardSize b => b -> BoardDirection -> Label -> Maybe Label
neighbour b dir (Label col row) =
    case dir of
      UpLeft -> check (col-1) (row+1)
      UpRight -> check (col+1) (row+1)
      DownLeft -> check (col-1) (row-1)
      DownRight -> check (col+1) (row-1)
      Up -> check col (row+1)
      ToRight -> check (col+1) row
      Down -> check col (row-1)
      ToLeft -> check (col-1) row
  where
    check c r
      | (c < 0) || (c >= ncols) || (r < 0) || (r >= nrows) = Nothing
      | otherwise = Just (Label c r)
    (nrows, ncols) = boardSize b

myNeighbour :: (HasBoardOrientation g, HasBoardSize g) => g -> Side -> PlayerDirection -> Label -> Maybe Label
myNeighbour g side dir a = neighbour g (myDirection g side dir) a

getNeighbourDirection :: HasBoardSize b => b -> Label -> Label -> Maybe BoardDirection
getNeighbourDirection b src dst = go allBoardDirections
  where
    go [] = Nothing
    go (dir : dirs)
      | neighbour b dir src == Just dst = Just dir
      | otherwise = go dirs

isValidDirection :: HasBoardSize b => b -> BoardDirection -> Label -> Bool
isValidDirection b dir a = isJust (neighbour b dir a)

getNeighbourPiece :: BoardDirection -> Label -> Board -> Maybe Piece
getNeighbourPiece dir addr board = do
  addr' <- neighbour board dir addr
  getPiece addr' board

inDirection :: HasBoardSize b => b -> BoardDirection -> Label -> Int -> Maybe Label
inDirection b _ src 0 = Just src
inDirection b dir src 1 = neighbour b dir src
inDirection b dir src n = neighbour b dir =<< inDirection b dir src (n-1)

getPieceInDirection :: HasBoardSize b => b -> BoardDirection -> Label -> Board -> Int -> Maybe Piece
getPieceInDirection b dir src board n = do
  dst <- inDirection b dir src n
  getPiece dst board

isLastHorizontal :: (HasBoardSize b, HasBoardOrientation b) => b -> Side -> Label -> Bool
isLastHorizontal b side (Label col row) =
  case boardOrientation b of
    FirstAtBottom -> (side == First) == (row == nrows-1)
    SecondAtBottom -> (side == Second) == (row == 0)
  where
    (nrows, ncols) = boardSize b

isWithinBoard :: (HasBoardOrientation rules, HasBoardSize board) => rules -> Side -> board -> Move -> Bool
isWithinBoard rules side board move = go (moveBegin move) (moveSteps move)
  where
    go _ [] = True
    go addr (step : steps) =
      case neighbour board (myDirection rules side (sDirection step)) addr of
        Just addr' -> go addr' steps
        Nothing -> False

allPassedAddresses :: (HasBoardOrientation rules, HasBoardSize board) => rules -> Side -> board -> Move -> [Label]
allPassedAddresses rules side board move = moveBegin move : (reverse $ go [] (moveBegin move) (moveSteps move))
  where
    go acc _ [] = acc
    go acc addr (step : steps) =
      case neighbour board (myDirection rules side (sDirection step)) addr of
        Just addr' -> go (addr' : acc) addr' steps
        Nothing -> error $ "allPassedAddresses: invalid step: " ++ show step

allPassedLabels :: (HasBoardOrientation rules, HasBoardSize board) => rules -> Side -> board -> Move -> [Label]
allPassedLabels rules side board move = allPassedAddresses rules side board move

nonCaptureLabels :: GameRules rules => rules -> Side -> Board -> Move -> [Label]
nonCaptureLabels rules side board move =
    moveBegin move : (reverse $ go [] (moveBegin move) (moveSteps move))
  where
    go acc _ [] = acc
    go acc addr (step : steps) =
      case neighbour board (myDirection rules side (sDirection step)) addr of
        Just addr' ->
          if sCapture step
            then go acc addr' steps
            else go (addr' : acc) addr' steps
        Nothing -> error $ "nonCaptureLabels: invalid step: " ++ show step

isMyPiece :: Side -> Piece -> Bool
isMyPiece side (Piece _ s) = side == s

isOpponentPiece :: Side -> Piece -> Bool
isOpponentPiece side (Piece _ s) = side /= s

isMyPieceAt :: Side -> Label -> Board -> Bool
isMyPieceAt side addr board =
  case getPiece addr board of
    Nothing -> False
    Just piece -> isMyPiece side piece

isOpponentAt :: Side -> Label -> Board -> Bool
isOpponentAt side addr board =
  case getPiece addr board of
    Nothing -> False
    Just piece -> isOpponentPiece side piece

isFree :: Label -> Board -> Bool
isFree addr b =
  not $ addr `labelSetMember` bOccupied b

isFreeInDirection :: BoardDirection -> Label -> Board -> Int -> Bool
isFreeInDirection dir src board n =
  case inDirection board dir src n of
    Nothing -> False
    Just dst -> isNothing (getPiece dst board)

allMyLabels :: Side -> Board -> [Label]
allMyLabels side board = myMen side board ++ myKings side board

myMen :: Side -> Board -> [Label]
myMen First board = labelSetToList $ bFirstMen board
myMen Second board = labelSetToList $ bSecondMen board

{-# DEPRECATED #-}
myMenA :: Side -> Board -> [Label]
myMenA side board = myMen side board

myKings :: Side -> Board -> [Label]
myKings First board = labelSetToList $ bFirstKings board
myKings Second board = labelSetToList $ bSecondKings board

{-# DEPRECATED #-}
myKingsA :: Side -> Board -> [Label]
myKingsA side board = myKings side board

allMyAddresses :: Side -> Board -> [Label]
allMyAddresses side board = allMyLabels side board

allMyPieces :: Side -> Board -> [(Label, PieceKind)]
allMyPieces side board =
  [(l, King) | l <- myKings side board] ++
  [(l, Man) | l <- myMen side board]

myLabelsCount :: Side -> Board -> (Label -> Bool) -> (Int, Int)
myLabelsCount side board p =
  (length $ filter p $ myMen side board,
   length $ filter p $ myKings side board)

myLabelsCount' :: Integral i => Side -> Board -> (Label -> i) -> (i, i)
myLabelsCount' side board w =
  (sum $ map w $ myMen side board,
   sum $ map w $ myKings side board)

{-# DEPRECATED #-}
myAddressesCount' :: Integral i => Side -> Board -> (Label -> i) -> (i, i)
myAddressesCount' = myLabelsCount'

myCounts :: Side -> Board -> (Int, Int)
myCounts side board =
  case side of
        First -> (IS.size (bFirstMen board), IS.size (bFirstKings board))
        Second -> (IS.size (bSecondMen board), IS.size (bSecondKings board))

totalCount :: Board -> Int
totalCount b =
    IS.size (bFirstMen b) +
    IS.size (bSecondMen b) +
    IS.size (bFirstKings b) +
    IS.size (bSecondKings b)

catMoves :: Move -> Move -> Move
catMoves m1 m2 =
  Move (moveBegin m1) (moveSteps m1 ++ moveSteps m2)

catPMoves :: PossibleMove -> PossibleMove -> PossibleMove
catPMoves pm1 pm2 = 
      PossibleMove {
        pmBegin = pmBegin pm1,
        pmEnd = pmEnd pm2,
        pmVictims = pmVictims pm1 ++ pmVictims pm2,
        pmVictimsCount = pmVictimsCount pm1 + pmVictimsCount pm2,
        pmMove = catMoves (pmMove pm1) (pmMove pm2),
        pmPromote = pmPromote pm1 || pmPromote pm2,
        pmResult = cat (pmResult pm1) (pmResult pm2)
      }
  where
    cat lst1 lst2 =
      case (last lst1, head lst2) of
        (Put a1 _, Take a2) | a1 == a2 -> init lst1 ++ tail lst2
        _ -> lst1 ++ lst2

isCaptureM :: Move -> Bool
isCaptureM move = any sCapture (moveSteps move)

isCapture :: PossibleMove -> Bool
isCapture pm = not $ null $ pmVictims pm

isPromotionM :: Move -> Bool
isPromotionM move = any sPromote (moveSteps move)

isPromotion :: PossibleMove -> Bool
isPromotion = pmPromote

capturesCount :: Move -> Int
capturesCount move = length $ filter sCapture (moveSteps move)

capturesCounts :: GameRules rules => rules -> Move -> Board -> (Int, Int)
capturesCounts rules move board =
--   trace (printf "CC: %s" (show move)) $
  let captures = getCaptured rules move board
      (men, kings) = partition isMan $ map snd captures
  in  (length men, length kings)

applyStep :: HasBoardOrientation rules => rules -> Piece -> Label -> Step -> Board -> (Board, Label, Piece)
applyStep rules piece@(Piece _ side) src step board =
  case neighbour board (myDirection rules side (sDirection step)) src of
    Nothing -> error $ "no such neighbour: " ++ show step
    Just dst ->
      let piece' = if sPromote step
                     then Piece King side
                     else piece
          board' = setPiece dst piece' $ removePiece src board
      in  (board', dst, piece')

--   case checkWellFormedStep piece board src step of
--     ValidStep dst ->
--         let piece' = if sPromote step
--                        then Piece King side
--                        else piece
--             board' = setPiece dst piece' $ removePiece src board
--         in (board', dst, piece')
--     err -> error $ printf "applyStep: Step is not well-formed: [%s at %s]: %s: %s" (show piece) (show src) (show step) (show err)

applyMove :: HasBoardOrientation rules => rules -> Side -> Move -> Board -> (Board, Label, Piece)
applyMove rules side move board = go board piece (moveBegin move) (moveSteps move)
  where
    go b p src [] = (b, src, p)
    go b p src (step : steps) =
      let (b', dst, p') = applyStep rules p src step b
      in  go b' p' dst steps

    piece = getPiece_ "applyMove" (moveBegin move) board

applyMoveAction :: MoveAction -> Board -> Either String Board
applyMoveAction (Take a) b =
  if isFree a b
    then Left $ printf "Take: no piece at %s; board: %s" (show a) (show b)
    else Right $ removePiece a b
applyMoveAction (Put a p) b = Right $ setPiece a p b
applyMoveAction (RemoveCaptured a) b =
  if isFree a b
    then Left $ printf "RemoveCaptured: no piece at %s; board: %s" (show a) (show b)
    else Right $ removePiece a b
applyMoveAction (MarkCaptured a) b =
  if isFree a b
    then Left $ printf "MarkCaptured: no piece at %s; board: %s" (show a) (show b)
    else if isCaptured a b
           then Left $ printf "MarkCaptured: piece at %s was already captured; board: %s" (show a) (show b)
           else Right $ b {bCaptured = insertLabelSet a (bCaptured b)}

applyMoveActions' :: [MoveAction] -> Board -> Either String Board
applyMoveActions' actions board = do
  board' <- foldM (flip applyMoveAction) board actions
  let board'' = foldr removePiece' board' (labelSetToList $ bCaptured board')
  return $ board'' {bCaptured = emptyLabelSet}

applyMoveActions :: [MoveAction] -> Board -> Board
applyMoveActions actions board =
  case applyMoveActions' actions board of
    Left err -> error $ printf "applyMoveActions: %s; actions: %s; board: %s" err (show actions) (show board)
    Right result -> result

isCaptured :: Label -> Board -> Bool
isCaptured a b = a `labelSetMember` bCaptured b

getCaptured :: GameRules rules => rules -> Move -> Board -> [(Label, Piece)]
getCaptured rules move board = go board (moveBegin move) (moveSteps move)
  where
    me = getPiece_ "getCaptured: me" (moveBegin move) board

    go _ _ [] = []
    go board addr (step : steps) =
      if sCapture step
        then let victim = getPiece_ "getCaptured" addr' board
                 (board', addr', _) = applyStep rules me addr step board
             in (addr, victim) : go board' addr' steps
        else let (board', addr', _) = applyStep rules me addr step board
             in go board' addr' steps

moveEnd :: GameRules rules => rules -> Side -> Board -> Move -> Label
moveEnd rules side board move = last $ allPassedAddresses rules side board move

simpleMove :: (HasBoardOrientation b, HasBoardSize b) => b -> Side -> Label -> PlayerDirection -> Move
simpleMove rules side src dir = Move src [Step dir False promote]
  where
    promote = case neighbour rules (myDirection rules side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal rules side dst

simpleCapture :: (HasBoardOrientation b, HasBoardSize b) => b -> Side -> Label -> PlayerDirection -> Move
simpleCapture rules side src dir = Move src [Step dir True False, Step dir False promote]
  where
    promote = case neighbour rules (myDirection rules side dir) =<< neighbour rules (myDirection rules side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal rules side dst

kingMove :: Side -> Label -> PlayerDirection -> Int -> Move
kingMove side src dir n = Move src $ replicate n (Step dir False False)

firstMoveDirection :: Move -> PlayerDirection
firstMoveDirection move = sDirection $ head $ moveSteps move

calcBoardHash :: Board -> BoardHash
calcBoardHash board = foldr update 0 (boardAssocs board)
  where
    update (label, piece) hash = updateBoardHash' table hash label piece
    table = randomTable board

updateBoardHash' :: RandomTable -> BoardHash -> Label -> Piece -> BoardHash
updateBoardHash' table hash (Label col row) piece =
  hash `xor` (table A.! (unboxPiece (Just piece), mkIndex col row))

updateBoardHash :: Board -> Label -> Piece -> BoardHash
updateBoardHash board label piece =
  updateBoardHash' (randomTable board) (boardHash board) label piece

buildBoard :: (RandomTableProvider rnd, HasTopology rules, HasBoardOrientation rules) => rnd -> rules -> BoardOrientation -> BoardSize -> Board
buildBoard rnd rules orient bsize@(nrows, ncols) =
  let table = getRandomTable rnd
      board = Board {
                bCaptured = emptyLabelSet,
                bOccupied = emptyLabelSet,
                bFirstMen = emptyLabelSet,
                bSecondMen = emptyLabelSet,
                bFirstKings = emptyLabelSet,
                bSecondKings = emptyLabelSet,
                bFirstAttacked = emptyLabelSet,
                bSecondAttacked = emptyLabelSet,
                bOrientation = boardOrientation rules,
                bSize = bsize,
                boardHash = 0,
                randomTable = table
              }

  in  board

getPiece :: Label -> Board -> Maybe Piece
getPiece a b
  | a `labelSetMember` bFirstKings b = Just $ Piece King First
  | a `labelSetMember` bSecondKings b = Just $ Piece King Second
  | a `labelSetMember` bFirstMen b = Just $ Piece Man First
  | a `labelSetMember` bSecondMen b = Just $ Piece Man Second
  | otherwise = Nothing

isPieceAt :: Label -> Board -> Side -> Bool
isPieceAt label b side =
  case side of
    First -> label `labelSetMember` bFirstMen b || label `labelSetMember` bFirstKings b
    Second -> label `labelSetMember` bSecondMen b || label `labelSetMember` bSecondKings b

getPiece_ :: String -> Label -> Board -> Piece
getPiece_ name addr board =
  case getPiece addr board of
    Nothing -> error $ name ++ ": no piece at " ++ show addr
    Just piece -> piece

{-# DEPRECATED #-}
getPiece' :: Label -> Board -> Maybe Piece
getPiece' = getPiece

getPiecesCount :: Piece -> LabelSet -> Board -> Int
getPiecesCount (Piece King First) set board = labelSetSize $ intersectLabelSet set (bFirstKings board)
getPiecesCount (Piece King Second) set board = labelSetSize $ intersectLabelSet set (bSecondKings board)
getPiecesCount (Piece Man First) set board = labelSetSize $ intersectLabelSet set (bFirstMen board)
getPiecesCount (Piece Man Second) set board = labelSetSize $ intersectLabelSet set (bSecondMen board)

getCapturablePiece :: Label -> Board -> Maybe Piece
getCapturablePiece a b =
  if isCaptured a b
    then Nothing
    else getPiece a b

setPiece :: Label -> Piece -> Board -> Board
setPiece a p b = board
  where
    b1 = if isFree a b
           then b
           else removePiece a b
    b2 = case getPiece a b of
           Nothing -> insertBoard a p b1
           Just old -> insertBoard a p $ removeBoard a old b1
    board = b2 {
              boardHash = updateBoardHash b1 a p
            }

removePiece :: Label -> Board -> Board
removePiece a b = board
  where
    board = case getPiece a b of
              Nothing -> error $ printf "removePiece: there is no piece at %s; board: %s" (show a) (show b)
              Just piece ->
                let b1 = removeBoard a piece b
                    b2 = b1 {
                          boardHash = updateBoardHash b a piece
                        }
                in  b2

{-# DEPRECATED #-}
removePiece' :: Label -> Board -> Board
removePiece' = removePiece

movePiece :: Label -> Label -> Board -> Board
movePiece src dst board =
  case getPiece src board of
    Nothing -> error $ "movePiece: no piece at " ++ show src
    Just piece -> setPiece dst piece $ removePiece src board

{-# DEPRECATED #-}
movePiece' :: Label -> Label -> Board -> Board
movePiece' = movePiece

{-# DEPRECATED #-}
setPiece' :: Label -> Piece -> Board -> Board
setPiece' = setPiece

setManyPieces :: [Label] -> Piece -> Board -> Board
setManyPieces addresses piece board = foldr (\a b -> setPiece a piece b) board addresses

{-# DEPRECATED #-}
setManyPieces' :: [Label] -> Piece -> Board -> Board
setManyPieces' = setManyPieces

line1labels :: [Label]
line1labels = ["a1", "c1", "e1", "g1"]

line2labels :: [Label]
line2labels = ["b2", "d2", "f2", "h2"]

line3labels :: [Label]
line3labels = ["a3", "c3", "e3", "g3"]

line4labels :: [Label]
line4labels = ["b4", "d4", "f4", "h4"]

line5labels :: [Label]
line5labels = ["a5", "c5", "e5", "g5"]

line6labels :: [Label]
line6labels = ["b6", "d6", "f6", "h6"]

line7labels :: [Label]
line7labels = ["a7", "c7", "e7", "g7"]

line8labels :: [Label]
line8labels = ["b8", "d8", "f8", "h8"]

emptyBoard8 :: (RandomTableProvider rnd, HasTopology rules, HasBoardOrientation rules) => rnd -> rules -> Board
emptyBoard8 rnd rules = buildBoard rnd rules FirstAtBottom (8, 8)

board8 :: (RandomTableProvider rnd, HasTopology rules, HasBoardOrientation rules) => rnd -> rules -> Board
board8 rnd rules =
  let board = buildBoard rnd rules FirstAtBottom (8, 8)
      labels1 = line1labels ++ line2labels ++ line3labels
      labels2 = line8labels ++ line7labels ++ line6labels
  in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

moveRep :: (HasBoardOrientation rules, HasBoardSize rules) => rules -> Side -> Move -> MoveRep
moveRep rules side move = FullMoveRep (moveBegin move) $ rep (moveBegin move) (moveSteps move)
  where

    rep _ [] = []
    rep prev (step@(Step dir capture promote) : steps) =
      case neighbour rules (myDirection rules side dir) prev of
        Nothing -> error $ "moveRep: invalid step: " ++ show step
        Just addr -> (StepRep addr capture promote) : rep addr steps

parseMoveRep :: GameRules rules => rules -> Side -> Board -> MoveRep -> MoveParseResult
parseMoveRep rules side board (ShortMoveRep from to) =
  let moves = possibleMoves rules side board
      suits pm = (pmBegin pm) == from &&
                (pmEnd pm) == to
  in  case filter suits moves of
        [pm] -> Parsed $ pmMove pm
        [] -> NoSuchMove
        ms -> AmbigousMove ms
parseMoveRep rules side board (FullMoveRep from steps) =
      Parsed $ Move from $ parse from steps
--     case lookupLabel from (bAddresses board) of
--       Nothing -> NoSuchMove
--       Just src -> Parsed $ Move src $ parse src steps
  where
    parse _ [] = []
    parse prev (step@(StepRep dst capture promote) : steps) =
      case getNeighbourDirection board prev dst of
        Nothing -> error $ "parseMoveRep: invalid step: " ++ show step
        Just dir -> Step (playerDirection side dir) capture promote : parse dst steps

boardAssocs :: Board -> [(Label, Piece)]
boardAssocs board = 
    [(label, Piece Man First) | label <- labelSetToList (bFirstMen board)] ++
    [(label, Piece Man Second) | label <- labelSetToList (bSecondMen board)] ++
    [(label, Piece King First) | label <- labelSetToList (bFirstKings board)] ++
    [(label, Piece King Second) | label <- labelSetToList (bSecondKings board)]

boardRep :: Board -> BoardRep
boardRep board = BoardRep $ boardAssocs board

parseBoardRep :: (GameRules rules, RandomTableProvider rnd) => rnd -> rules -> BoardRep -> Board
parseBoardRep rnd rules (BoardRep list) = foldr set (buildBoard rnd rules orient bsize) list
  where
    set (label, piece) board = setPiece' label piece board
    bsize = boardSize rules
    orient = boardOrientation rules

-- | Generic implementation of @getGameResult@, which suits most rules.
-- This can not, however, recognize draws.
genericGameResult :: GameRules rules => rules -> GameState -> Board -> Side -> Maybe GameResult
genericGameResult rules st board side
  | side == First && null (possibleMoves rules First board) = Just SecondWin
  | side == Second && null (possibleMoves rules Second board) = Just FirstWin
  | detectRepeatedPosition 3 (gsHistory st) board = Just Draw
  | detectStalemate (gsHistory st) board = Just Draw
  | otherwise = Nothing

detectRepeatedPosition :: Int -> [HistoryRecord] -> Board -> Bool
detectRepeatedPosition n history board =
  case history of
    [] -> False
    [_] -> False
    (record : prevRecord: history') ->
      if hrPrevBoard record == board
        then detectRepeatedPosition (n-1) history' (hrPrevBoard record)
        else False

detectStalemate :: [HistoryRecord] -> Board -> Bool
detectStalemate history board
  | totalCount board <= 3 = detectStalemate' 5 history board
  | totalCount board <= 5 = detectStalemate' 30 history board
  | totalCount board <= 7 = detectStalemate' 60 history board
  | otherwise = False
      
detectStalemate' :: Int -> [HistoryRecord] -> Board -> Bool
detectStalemate' nMoves history board =
  let counts = calcBoardCounts board
      change r = isCaptureM (hrMove r) || isPromotionM (hrMove r)
      nHalfMoves = 2*nMoves
  in  if bcFirstKings counts > 0 && bcSecondKings counts > 0
        then  length history > nHalfMoves &&
                (not $ any change $ take nHalfMoves history)
        else False

instance IsString Label where
  fromString str =
    case parseChessNotationS str of
      Left err -> error err
      Right label -> label
    
-- | Chess-like fields notation, like "A1" or "H8"
chessNotation :: Label -> Notation
chessNotation = T.pack . map toUpper . show

-- | Parse chess-like fields notation.
parseChessNotation :: Notation -> Either String Label
parseChessNotation = parseChessNotationS . T.unpack

-- | Parse chess-like fields notation.
parseChessNotationS :: String -> Either String Label
parseChessNotationS = parse . map toLower
  where
    parse (l:ds)
      | all isDigit ds =
        case elemIndex l letters of
          Nothing -> Left $ "parseChessNotation: unknown letter: " ++ [l]
          Just col -> let row = read ds - 1
                      in  Right $ Label (fromIntegral col) row
    parse e = Left $ "parseChessNotation: cant parse: " ++ e

-- | Numeric (international) fields notation
numericNotation :: BoardSize -> Label -> Notation
numericNotation (nrows, ncols) (Label col row) =
  let half = ncols `div` 2
      row' = nrows - row - 1
      n = row' * half + (col `div` 2) + 1
  in  T.pack $ show n

-- | Parse numeric (international) fields notation
parseNumericNotation :: BoardSize -> Notation -> Either String Label
parseNumericNotation (nrows, ncols) t = parse (T.unpack t)
  where
    parse str
      | all isDigit str =
        let n = read str - 1
            half = ncols `div` 2
            row' = n `div` half
            col' = n `mod` half
            row = ncols - row' - 1
            col = if odd row
                    then col'*2 + 1
                    else col'*2
        in  Right $ Label col row

      | otherwise = Left $ "parseNumericNotation: Cant parse: " ++ str

chessSideNotation :: BoardSize -> SideNotation
chessSideNotation (nrows, ncols) =
  let row = [Just (T.pack [toUpper letter]) | letter <- take (fromIntegral ncols) letters]
      column = [Just (T.pack $ show row) | row <- [1..nrows]]
  in  SideNotation {
        snTopLabels = row,
        snLeftLabels = column,
        snBottomLabels = row,
        snRightLabels = column
      }

numericSideNotation :: BoardSize -> SideNotation
numericSideNotation (nrows, ncols) =
    SideNotation {
      snTopLabels = sparse True [1..halfCols]
    , snLeftLabels = reverse $ sparse True $ take halfRows [halfCols + 1, halfCols + ncols + 1 ..]
    , snBottomLabels = sparse False [n - halfCols + 1 .. n]
    , snRightLabels = reverse $ sparse False $ take halfRows [halfCols, halfCols + ncols ..]
    }
  where
    halfCols = fromIntegral $ ncols `div` 2
    halfRows = fromIntegral $ nrows `div` 2
    n = (nrows * ncols) `div` 2

    sparse True numbers = concat [[Nothing, Just (T.pack $ show i)] | i <- numbers]
    sparse False numbers = concat [[Just (T.pack $ show i), Nothing] | i <- numbers]

flipBoardKey :: BoardSize -> BoardKey -> BoardKey
flipBoardKey (nrows,ncols) bk =
    IM.fromList $ map go $ IM.assocs bk
  where
    go (k, p) = (labelIndex $ flipLabel $ unpackIndex k, opponentPiece p)
    flipLabel (Label col row) = Label (ncols - col - 1) (nrows - row - 1)

flipBoardCounts :: BoardCounts -> BoardCounts
flipBoardCounts bc =
  bc {
    bcFirstMen = bcSecondMen bc,
    bcSecondMen = bcFirstMen bc,
    bcFirstKings = bcSecondKings bc,
    bcSecondKings = bcFirstKings bc
  }

flipBoard :: Board -> Board
flipBoard b = b' {boardHash = hash}
  where
    b' = b {
      bFirstMen = labelSetFromList $ map flipLabel (labelSetToList $ bSecondMen b),
      bSecondMen = labelSetFromList $ map flipLabel (labelSetToList $ bFirstMen b),
      bFirstKings = labelSetFromList $ map flipLabel (labelSetToList $ bSecondKings b),
      bSecondKings = labelSetFromList $ map flipLabel (labelSetToList $ bFirstKings b),
      bOccupied =  labelSetFromList $ map flipLabel (labelSetToList $ bOccupied b)
      -- boardCounts = flipBoardCounts (boardCounts b)
    }

    hash = calcBoardHash b'

    (nrows, ncols) = bSize b

    flipLabel (Label col row) = Label (ncols - col - 1) (nrows - row - 1)

boardAttacked :: Side -> Board -> LabelSet
boardAttacked First = bFirstAttacked
boardAttacked Second = bSecondAttacked

markAttacked :: [PossibleMove] -> Board -> Board
markAttacked moves board =
  let attackedBy side = labelSetFromList $ concatMap pmVictims moves
  in  board {
        bFirstAttacked = attackedBy Second,
        bSecondAttacked = attackedBy First
      }

