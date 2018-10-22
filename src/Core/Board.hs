{-# LANGUAGE OverloadedStrings #-}
module Core.Board where

import Data.Maybe
import Data.List
import Data.String
import Data.Char (isDigit, toLower, toUpper)
import qualified Data.Map as M
import qualified Data.Text as T
import Text.Printf

-- import Debug.Trace

import Core.Types
import Core.BoardMap

showAddress :: Address -> String
showAddress a =
  printf "%s {UL: %s, UR: %s, DL: %s, DR: %s}"
          (show $ aLabel a)
          (maybe "X" (show . aLabel) $ aUpLeft a)
          (maybe "X" (show . aLabel) $ aUpRight a)
          (maybe "X" (show . aLabel) $ aDownLeft a)
          (maybe "X" (show . aLabel) $ aDownRight a)

showAddress2 :: Address -> String
showAddress2 a =
  printf "%s {UL: (%s), UR: (%s), DL: (%s), DR: (%s)}"
          (show $ aLabel a)
          (maybe "X" showAddress $ aUpLeft a)
          (maybe "X" showAddress $ aUpRight a)
          (maybe "X" showAddress $ aDownLeft a)
          (maybe "X" showAddress $ aDownRight a)

opposite :: Side -> Side
opposite First = Second
opposite Second = First

isMan :: Piece -> Bool
isMan (Piece kind _) = kind == Man

isKing :: Piece -> Bool
isKing (Piece kind _) = kind == King

myDirection :: Side -> PlayerDirection -> BoardDirection
myDirection First ForwardLeft = UpLeft
myDirection First ForwardRight = UpRight
myDirection First BackwardLeft = DownLeft
myDirection First BackwardRight = DownRight
myDirection Second ForwardLeft = DownRight
myDirection Second ForwardRight = DownLeft
myDirection Second BackwardLeft = UpRight
myDirection Second BackwardRight = UpLeft

playerDirection :: Side -> BoardDirection -> PlayerDirection
playerDirection First UpLeft = ForwardLeft
playerDirection First UpRight = ForwardRight
playerDirection First DownLeft = BackwardLeft
playerDirection First DownRight = BackwardRight
playerDirection Second UpLeft = BackwardRight
playerDirection Second UpRight = BackwardLeft
playerDirection Second DownLeft = ForwardRight
playerDirection Second DownRight = ForwardLeft

oppositeDirection :: PlayerDirection -> PlayerDirection
oppositeDirection ForwardLeft = BackwardRight
oppositeDirection ForwardRight = BackwardLeft
oppositeDirection BackwardLeft = ForwardRight
oppositeDirection BackwardRight = ForwardLeft

neighbour :: BoardDirection -> Address -> Maybe Address
neighbour UpLeft a = aUpLeft a
neighbour UpRight a = aUpRight a
neighbour DownLeft a = aDownLeft a
neighbour DownRight a = aDownRight a

getNeighbourDirection :: Address -> Address -> Maybe BoardDirection
getNeighbourDirection src dst
  | aUpLeft src == Just dst = Just UpLeft
  | aUpRight src == Just dst = Just UpRight
  | aDownLeft src == Just dst = Just DownLeft
  | aDownRight src == Just dst = Just DownRight
  | otherwise = Nothing

getNeighbourDirection' :: Board -> Address -> Label -> Maybe BoardDirection
getNeighbourDirection' board src dst =
  getNeighbourDirection src (resolve dst board)

isValidDirection :: BoardDirection -> Address -> Bool
isValidDirection dir a = isJust (neighbour dir a)

getNeighbourPiece :: BoardDirection -> Address -> Board -> Maybe Piece
getNeighbourPiece dir addr board = do
  addr' <- neighbour dir addr
  getPiece addr' board

inDirection :: BoardDirection -> Address -> Int -> Maybe Address
inDirection _ src 0 = Just src
inDirection dir src 1 = neighbour dir src
inDirection dir src n = neighbour dir =<< inDirection dir src (n-1)

getPieceInDirection :: BoardDirection -> Address -> Board -> Int -> Maybe Piece
getPieceInDirection dir src board n = do
  dst <- inDirection dir src n
  getPiece dst board

isLastHorizontal :: Side -> Address -> Bool
isLastHorizontal side a =
  aPromotionSide a == Just side

isWithinBoard :: Side -> Board -> Move -> Bool
isWithinBoard side board move = go (moveBegin move) (moveSteps move)
  where
    go _ [] = True
    go addr (step : steps) =
      case neighbour (myDirection side (sDirection step)) addr of
        Just addr' -> go addr' steps
        Nothing -> False

allPassedAddresses :: Side -> Board -> Move -> [Address]
allPassedAddresses side board move = reverse $ go [] (moveBegin move) (moveSteps move)
  where
    go acc _ [] = acc
    go acc addr (step : steps) =
      case neighbour (myDirection side (sDirection step)) addr of
        Just addr' -> go (addr' : acc) addr' steps
        Nothing -> error $ "allPassedAddresses: invalid step: " ++ show step

isMyPiece :: Side -> Piece -> Bool
isMyPiece side (Piece _ s) = side == s

isOpponentPiece :: Side -> Piece -> Bool
isOpponentPiece side (Piece _ s) = side /= s

isMyPieceAt :: Side -> Address -> Board -> Bool
isMyPieceAt side addr board =
  case getPiece addr board of
    Nothing -> False
    Just piece -> isMyPiece side piece

isOpponentAt :: Side -> Address -> Board -> Bool
isOpponentAt side addr board =
  case getPiece addr board of
    Nothing -> False
    Just piece -> isOpponentPiece side piece

isFree :: Address -> Board -> Bool
isFree addr board = isNothing (getPiece addr board)

isFreeInDirection :: BoardDirection -> Address -> Board -> Int -> Bool
isFreeInDirection dir src board n =
  case inDirection dir src n of
    Nothing -> False
    Just dst -> isNothing (getPiece dst board)

allMyAddresses :: Side -> Board -> [Address]
allMyAddresses side board =
  map (\l -> resolve l board) $ findLabels (isMyPiece side) (bPieces board)

myCounts :: Side -> Board -> (Int, Int)
myCounts side board =
  let counts = boardCounts board
  in  case side of
        First -> (bcFirstMen counts, bcFirstKings counts)
        Second -> (bcSecondMen counts, bcSecondKings counts)

checkWellFormedStep :: Piece -> Board -> Address -> Step -> StepCheckResult
checkWellFormedStep (Piece kind side) board src step =
  case neighbour (myDirection side (sDirection step)) src of
    Nothing -> NoSuchNeighbour
    Just dst ->
      if sCapture step
        then -- trace (printf "Dir: %s, Capture: %s, Dst: %s, piece: %s" (show $ myDirection side (sDirection step)) (show $ sCapture step) (show dst) (show $ getPiece dst board)) $
             case getPiece dst board of
               Nothing -> NoPieceToCapture
               Just piece -> if isMyPiece side piece
                               then CapturingOwnPiece
                               else ValidStep dst
        else if isJust (getPiece dst board)
               then OccupatedField
               else if (kind == Man) && (sPromote step /= isLastHorizontal side dst)
                       then InvalidPromotion (sPromote step) (isLastHorizontal side dst)
                       else ValidStep dst

isWellFormedMove :: Piece -> Board -> Move -> MoveCheckResult
isWellFormedMove piece board move =
    -- isMyPieceAt side (moveBegin move) board &&
    go (moveBegin move) (moveSteps move)
  where
    go _ [] = ValidMove
    go src (step : steps) =
      case checkWellFormedStep piece board src step of
        ValidStep dst -> go dst steps
        err -> InvalidStep step err

catMoves :: Move -> Move -> Move
catMoves m1 m2 =
  Move (moveBegin m1) (moveSteps m1 ++ moveSteps m2)

isCapture :: Move -> Bool
isCapture move = any sCapture (moveSteps move)

capturesCount :: Move -> Int
capturesCount move = length $ filter sCapture (moveSteps move)

capturesCounts :: Move -> Board -> (Int, Int)
capturesCounts move board =
--   trace (printf "CC: %s" (show move)) $
  let captures = getCaptured move board
      (men, kings) = partition isMan $ map snd captures
  in  (length men, length kings)

applyStep :: Piece -> Address -> Step -> Board -> (Board, Address, Piece)
applyStep piece@(Piece _ side) src step board =
  case neighbour (myDirection side (sDirection step)) src of
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
-- 
applyMove :: Side -> Move -> Board -> (Board, Address, Piece)
applyMove side move board = go board piece (moveBegin move) (moveSteps move)
  where
    go b p src [] = (b, src, p)
    go b p src (step : steps) =
      let (b', dst, p') = applyStep p src step b
      in  go b' p' dst steps

    piece = getPiece_ "applyMove" (moveBegin move) board

getCaptured :: Move -> Board -> [(Address, Piece)]
getCaptured move board = go board (moveBegin move) (moveSteps move)
  where
    me = getPiece_ "getCaptured: me" (moveBegin move) board

    go _ _ [] = []
    go board addr (step : steps) =
      if sCapture step
        then let victim = getPiece_ "getCaptured" addr' board
                 (board', addr', _) = applyStep me addr step board
             in (addr, victim) : go board' addr' steps
        else let (board', addr', _) = applyStep me addr step board
             in go board' addr' steps

moveEnd :: Side -> Board -> Move -> Address
moveEnd side board move = last $ allPassedAddresses side board move

simpleMove :: Side -> Address -> PlayerDirection -> Move
simpleMove side src dir = Move src [Step dir False promote]
  where
    promote = case neighbour (myDirection side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal side dst

simpleCapture :: Side -> Address -> PlayerDirection -> Move
simpleCapture side src dir = Move src [Step dir True False, Step dir False promote]
  where
    promote = case neighbour (myDirection side dir) =<< neighbour (myDirection side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal side dst

kingMove :: Side -> Address -> PlayerDirection -> Int -> Move
kingMove side src dir n = Move src $ replicate n (Step dir False False)

firstMoveDirection :: Move -> PlayerDirection
firstMoveDirection move = sDirection $ head $ moveSteps move

makeLine :: [Label] -> [Address]
makeLine labels = map (\l -> Address l Nothing Nothing Nothing Nothing Nothing) labels

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

buildBoard :: BoardSize -> Board
buildBoard (nrows, ncols) =
  let mkAddress p = Address (label p) (promote p) (upLeft p) (upRight p) (downLeft p) (downRight p)
      label (r,c) = Label (c-1) (r-1)

      promote (r,_)
        | r == 1 = Just Second
        | r == nrows = Just First
        | otherwise = Nothing

      upLeft (r,c)
        | r+1 > nrows || c-1 < 1 = Nothing
        | otherwise = M.lookup (r+1, c-1) addresses

      upRight (r,c)
        | r+1 > nrows || c+1 > ncols = Nothing
        | otherwise = M.lookup (r+1, c+1) addresses

      downLeft (r,c)
        | r-1 < 1 || c-1 < 1 = Nothing
        | otherwise = M.lookup (r-1, c-1) addresses

      downRight (r,c)
        | r-1 < 1 || c+1 > ncols = Nothing
        | otherwise = M.lookup (r-1, c+1) addresses

      addresses = M.fromList [(p, mkAddress p) | p <- coordinates]

      odds n = [1, 3 .. n]
      evens n = [2, 4 .. n]
      coordinates = [(r, c) | r <- odds nrows, c <- odds ncols] ++ [(r, c) | r <- evens nrows, c <- evens ncols]

      addressByLabel = buildLabelMap nrows ncols [(label p, address) | (p, address) <- M.assocs addresses]

      board = Board (emptyAddressMap (nrows,ncols)) addressByLabel counts key

      counts = calcBoardCounts board
      key = calcBoardKey board

  in  board

resolve :: Label -> Board -> Address
resolve label board = fromMaybe (error $ "resolve: unknown field: " ++ show label) $ lookupLabel label (bAddresses board)

getPiece :: Address -> Board -> Maybe Piece
getPiece a b = lookupAddress a (bPieces b)

getPiece_ :: String -> Address -> Board -> Piece
getPiece_ name addr board =
  case getPiece addr board of
    Nothing -> error $ name ++ ": no piece at " ++ show addr
    Just piece -> piece

getPiece' :: Label -> Board -> Maybe Piece
getPiece' l b = getPiece a b
  where
    a = fromMaybe (error $ "getPiece': unknown field: " ++ show l) $ lookupLabel l (bAddresses b)

setPiece :: Address -> Piece -> Board -> Board
setPiece a p b = board
  where
    board = b {bPieces = setAddress a p (bPieces b), boardCounts = counts, boardKey = key}
    counts = case lookupAddress a (bPieces b) of
               Nothing -> insertBoardCounts p (boardCounts b)
               Just old -> insertBoardCounts p $ removeBoardCounts old (boardCounts b)
    key = calcBoardKey board

removePiece :: Address -> Board -> Board
removePiece a b = board
  where
    updateMap addr piece = Nothing
    board = case lookupAddress a (bPieces b) of
              Nothing -> b
              Just piece -> b {bPieces = removeAddress a (bPieces b), boardCounts = removeBoardCounts piece (boardCounts b)}
    key = calcBoardKey board

removePiece' :: Label -> Board -> Board
removePiece' l b = removePiece (resolve l b) b

movePiece :: Address -> Address -> Board -> Board
movePiece src dst board =
  case getPiece src board of
    Nothing -> error $ "movePiece: no piece at " ++ show src
    Just piece -> setPiece dst piece $ removePiece src board

movePiece' :: Label -> Label -> Board -> Board
movePiece' src dst board =
  movePiece (resolve src board) (resolve dst board) board

setPiece' :: Label -> Piece -> Board -> Board
setPiece' l p b = setPiece a p b
  where
    a = fromMaybe (error $ "setPiece': unknown field: " ++ show l) $ lookupLabel l (bAddresses b)

setManyPieces :: [Address] -> Piece -> Board -> Board
setManyPieces addresses piece board = foldr (\a b -> setPiece a piece b) board addresses

setManyPieces' :: [Label] -> Piece -> Board -> Board
setManyPieces' labels piece board = foldr (\l b -> setPiece' l piece b) board labels

board8 :: Board
board8 =
  let board = buildBoard (8, 8)
      labels1 = line1labels ++ line2labels ++ line3labels
      labels2 = line8labels ++ line7labels ++ line6labels
  in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

moveRep :: Side -> Move -> MoveRep
moveRep side move = FullMoveRep (aLabel $ moveBegin move) $ rep (moveBegin move) (moveSteps move)
  where

    rep _ [] = []
    rep prev (step@(Step dir capture promote) : steps) =
      case neighbour (myDirection side dir) prev of
        Nothing -> error $ "moveRep: invalid step: " ++ show step
        Just addr -> (StepRep (aLabel addr) capture promote) : rep addr steps

parseMoveRep :: GameRules rules => rules -> Side -> Board -> MoveRep -> MoveParseResult
parseMoveRep rules side board (ShortMoveRep from to) =
  let moves = possibleMoves rules side board
      suits m = aLabel (moveBegin m) == from &&
                aLabel (moveEnd side board m) == to
  in  case filter suits moves of
        [m] -> Parsed m
        [] -> NoSuchMove
        ms -> AmbigousMove ms
parseMoveRep rules side board (FullMoveRep from steps) =
    case lookupLabel from (bAddresses board) of
      Nothing -> NoSuchMove
      Just src -> Parsed $ Move src $ parse src steps
  where
    parse _ [] = []
    parse prev (step@(StepRep dst capture promote) : steps) =
      case getNeighbourDirection' board prev dst of
        Nothing -> error $ "parseMoveRep: invalid step: " ++ show step
        Just dir -> Step (playerDirection side dir) capture promote : parse (resolve dst board) steps

boardRep :: Board -> BoardRep
boardRep board = BoardRep $ occupiedLabels $ bPieces board

parseBoardRep :: GameRules rules => rules -> BoardRep -> Board
parseBoardRep rules (BoardRep list) = foldr set (buildBoard bsize) list
  where
    set (label, piece) board = setPiece' label piece board
    bsize = boardSize rules

-- | Generic implementation of @getGameResult@, which suits most rules.
-- This can not, however, recognize draws.
genericGameResult :: GameRules rules => rules -> Board -> GameResult
genericGameResult rules board =
  if null (possibleMoves rules First board)
    then SecondWin
    else if null (possibleMoves rules Second board)
           then FirstWin
           else Ongoing

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

