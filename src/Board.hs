module Board where

import Control.Monad
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Text.Printf

import Debug.Trace

import Types

showAddress :: Address -> String
showAddress a =
  printf "%s {UL: %s, UR: %s, DL: %s, DR: %s}"
          (aLabel a)
          (maybe "X" aLabel $ aUpLeft a)
          (maybe "X" aLabel $ aUpRight a)
          (maybe "X" aLabel $ aDownLeft a)
          (maybe "X" aLabel $ aDownRight a)

showAddress2 :: Address -> String
showAddress2 a =
  printf "%s {UL: (%s), UR: (%s), DL: (%s), DR: (%s)}"
          (aLabel a)
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

neighbour :: BoardDirection -> Address -> Maybe Address
neighbour UpLeft a = aUpLeft a
neighbour UpRight a = aUpRight a
neighbour DownLeft a = aDownLeft a
neighbour DownRight a = aDownRight a

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
  not (isValidDirection (myDirection side ForwardLeft) a) && not (isValidDirection (myDirection side ForwardRight) a)

isWithinBoard :: Side -> Board -> Move -> Bool
isWithinBoard side board move = go (moveBegin move) (moveSteps move)
  where
    go _ [] = True
    go addr (step : steps) =
      case neighbour (myDirection side (sDirection step)) addr of
        Just addr' -> go addr' steps
        Nothing -> False

allPassedAddresses :: Side -> Board -> Move -> [Address]
allPassedAddresses side board move = go [] (moveBegin move) (moveSteps move)
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
  M.keys $ M.filter (isMyPiece side) (bPieces board)

checkWellFormedStep :: Piece -> Board -> Address -> Step -> Maybe Address
checkWellFormedStep (Piece kind side) board src step =
  case neighbour (myDirection side (sDirection step)) src of
    Nothing -> Nothing
    Just dst ->
      if sCapture step
        then -- trace (printf "Dir: %s, Capture: %s, Dst: %s, piece: %s" (show $ myDirection side (sDirection step)) (show $ sCapture step) (show dst) (show $ getPiece dst board)) $
             case getPiece dst board of
               Nothing -> Nothing
               Just piece -> if isMyPiece side piece
                               then Nothing
                               else Just dst
        else if isJust (getPiece dst board)
               then Nothing
               else if (kind == Man) && (sPromote step /= isLastHorizontal side dst)
                       then Nothing
                       else Just dst

isWellFormedMove :: Piece -> Board -> Move -> Bool
isWellFormedMove piece board move =
    -- isMyPieceAt side (moveBegin move) board &&
    go (moveBegin move) (moveSteps move)
  where
    go _ [] = True
    go src (step : steps) =
      case checkWellFormedStep piece board src step of
        Nothing -> False
        Just dst -> go dst steps

catMoves :: Move -> Move -> Move
catMoves m1 m2 =
  Move (moveBegin m1) (moveSteps m1 ++ moveSteps m2)

isCapture :: Move -> Bool
isCapture move = any sCapture (moveSteps move)

capturesCount :: Move -> Int
capturesCount move = length $ filter sCapture (moveSteps move)

capturesCounts :: Move -> Board -> (Int, Int)
capturesCounts move board =
  -- trace (printf "CC: %s" (show move)) $
  let captures = getCaptured move board
      (men, kings) = partition isMan $ map snd captures
  in  (length men, length kings)

applyStep :: Piece -> Address -> Step -> Board -> (Board, Address, Piece)
applyStep piece@(Piece _ side) src step board =
  case checkWellFormedStep piece board src step of
    Nothing -> error $ printf "Step is not well-formed: [%s]: %s" (show src) (show step)
    Just dst ->
        let piece' = if sPromote step
                       then Piece King side
                       else piece
            board' = setPiece dst piece' $ removePiece src board
        in (board', dst, piece')

applyMove :: Side -> Move -> Board -> (Board, Address, Piece)
applyMove side move board = go board piece (moveBegin move) (moveSteps move)
  where
    go b p src [] = (b, src, p)
    go b p src (step : steps) =
      let (b', dst, p') = applyStep p src step b
      in  go b' p' dst steps

    piece = getPiece_ "applyMove" (moveBegin move) board

getCaptured :: Move -> Board -> [(Address, Piece)]
getCaptured move board = go (moveBegin move) (moveSteps move)
  where
    me = getPiece_ "getCaptured: me" (moveBegin move) board

    go _ [] = []
    go addr (step : steps) =
      if sCapture step
        then let victim = getPiece_ "getCaptured" addr' board
                 (_, addr', _) = applyStep me addr step board
             in (addr, victim) : go addr' steps
        else let (_, addr', _) = applyStep me addr step board
             in go addr' steps

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
    promote = case neighbour (myDirection side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal side dst

kingMove :: Side -> Address -> PlayerDirection -> Int -> Move
kingMove side src dir n = Move src $ replicate n (Step dir False False)

makeLine :: [String] -> [Address]
makeLine labels = map (\l -> Address l Nothing Nothing Nothing Nothing) labels

line1labels :: [String]
line1labels = ["a1", "c1", "e1", "g1"]

line2labels :: [String]
line2labels = ["b2", "d2", "f2", "h2"]

line3labels :: [String]
line3labels = ["a3", "c3", "e3", "g3"]

line4labels :: [String]
line4labels = ["b4", "d4", "f4", "h4"]

line5labels :: [String]
line5labels = ["a5", "c5", "e5", "g5"]

line6labels :: [String]
line6labels = ["b6", "d6", "f6", "h6"]

line7labels :: [String]
line7labels = ["a7", "c7", "e7", "g7"]

line8labels :: [String]
line8labels = ["b8", "d8", "f8", "h8"]

--   2
--  / 
-- 1  
linkPrimary :: Address -> Address -> (Address, Address)
linkPrimary a1 a2 = (a1', a2')
  where
    a1' = a1 {aUpRight = Just a2'}
    a2' = a2 {aDownLeft = Just a1'}
   

-- 2
--  \
--   1
linkSecondary :: Address -> Address -> (Address, Address)
linkSecondary a1 a2 = (a1', a2')
  where
    a1' = a1 {aUpLeft = Just a2'}
    a2' = a2 {aDownRight = Just a1'}

linkA :: Bool -> Address -> Address -> (Address, Address)
linkA True = linkPrimary
linkA False = linkSecondary

buildBoard :: Int -> Board
buildBoard size =
  let mkAddress p = Address (label p) (upLeft p) (upRight p) (downLeft p) (downRight p)
      letters = ['a' .. 'z']
      label (r,c) = (letters !! (c-1)) : show r

      upLeft (r,c)
        | r+1 > size || c-1 < 1 = Nothing
        | otherwise = M.lookup (r+1, c-1) addresses

      upRight (r,c)
        | r+1 > size || c+1 > 8 = Nothing
        | otherwise = M.lookup (r+1, c+1) addresses

      downLeft (r,c)
        | r-1 < 1 || c-1 < 1 = Nothing
        | otherwise = M.lookup (r-1, c-1) addresses

      downRight (r,c)
        | r-1 < 1 || c+1 > 8 = Nothing
        | otherwise = M.lookup (r-1, c+1) addresses

      addresses = M.fromList [(p, mkAddress p) | p <- coordinates]

      odds = [1, 3 .. size]
      evens = [2, 4 .. size]
      coordinates = [(r, c) | r <- odds, c <- odds] ++ [(r, c) | r <- evens, c <- evens]

      addressByLabel = M.mapKeys label addresses

  in  Board M.empty addressByLabel

resolve :: String -> Board -> Address
resolve label board = fromMaybe (error $ "resolve: unknown field: " ++ label) $ M.lookup label (bAddresses board)

getPiece :: Address -> Board -> Maybe Piece
getPiece a b = M.lookup a (bPieces b)

getPiece_ :: String -> Address -> Board -> Piece
getPiece_ name addr board =
  case getPiece addr board of
    Nothing -> error $ name ++ ": no piece at " ++ show addr
    Just piece -> piece

getPiece' :: String -> Board -> Maybe Piece
getPiece' l b = M.lookup a (bPieces b)
  where
    a = fromMaybe (error $ "getPiece': unknown field: " ++ l) $ M.lookup l (bAddresses b)

setPiece :: Address -> Piece -> Board -> Board
setPiece a p b = b {bPieces = M.insert a p (bPieces b)}

removePiece :: Address -> Board -> Board
removePiece a b = b {bPieces = M.delete a (bPieces b)}

removePiece' :: String -> Board -> Board
removePiece' l b = removePiece (resolve l b) b

movePiece :: Address -> Address -> Board -> Board
movePiece src dst board =
  case getPiece src board of
    Nothing -> error $ "movePiece: no piece at " ++ show src
    Just piece -> setPiece dst piece $ removePiece src board

movePiece' :: String -> String -> Board -> Board
movePiece' src dst board =
  movePiece (resolve src board) (resolve dst board) board

setPiece' :: String -> Piece -> Board -> Board
setPiece' l p b = b {bPieces = M.insert a p (bPieces b)}
  where
    a = fromMaybe (error $ "setPiece': unknown field: " ++ l) $ M.lookup l (bAddresses b)

setManyPieces :: [Address] -> Piece -> Board -> Board
setManyPieces addresses piece board = foldr (\a b -> setPiece a piece b) board addresses

setManyPieces' :: [String] -> Piece -> Board -> Board
setManyPieces' labels piece board = foldr (\l b -> setPiece' l piece b) board labels

board8 :: Board
board8 =
  let board = buildBoard 8
      labels1 = line1labels ++ line2labels ++ line3labels
      labels2 = line8labels ++ line7labels ++ line6labels
  in  setManyPieces' labels1 (Piece Man First) $ setManyPieces' labels2 (Piece Man Second) board

