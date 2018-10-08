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

myDirection :: Side -> PlayerDirection -> BoardDirection
myDirection First ForwardLeft = UpLeft
myDirection First ForwardRight = UpRight
myDirection First BackwardLeft = DownLeft
myDirection First BackwardRight = DownRight
myDirection Second ForwardLeft = DownRight
myDirection Second ForwardRight = DownLeft
myDirection Second BackwardLeft = UpRight
myDirection Second BackwardRight = DownLeft

neighbour :: BoardDirection -> Address -> Maybe Address
neighbour UpLeft a = aUpLeft a
neighbour UpRight a = aUpRight a
neighbour DownLeft a = aDownLeft a
neighbour DownRight a = aDownRight a

isValidDirection :: BoardDirection -> Address -> Bool
isValidDirection dir a = isJust (neighbour dir a)

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

isMyPieceAt :: Side -> Address -> Board -> Bool
isMyPieceAt side addr board =
  case getPiece addr board of
    Nothing -> False
    Just piece -> isMyPiece side piece

isFree :: Address -> Board -> Bool
isFree addr board = isNothing (getPiece addr board)

allMyAddresses :: Side -> Board -> [Address]
allMyAddresses side board =
  M.keys $ M.filter (isMyPiece side) (bPieces board)

checkWellFormedStep :: Side -> Board -> Address -> Step -> Maybe Address
checkWellFormedStep side board src step =
  case neighbour (myDirection side (sDirection step)) src of
    Nothing -> Nothing
    Just dst ->
      if sCapture step
        then case getPiece dst board of
               Nothing -> Nothing
               Just piece -> if isMyPiece side piece
                               then Nothing
                               else Just dst
        else if isJust (getPiece dst board)
               then Nothing
               else if sPromote step /= isLastHorizontal side dst
                       then Nothing
                       else Just dst

isWellFormedMove :: Side -> Board -> Move -> Bool
isWellFormedMove side board move =
    isMyPieceAt side (moveBegin move) board &&
    go (moveBegin move) (moveSteps move)
  where
    go _ [] = True
    go src (step : steps) =
      case checkWellFormedStep side board src step of
        Nothing -> False
        Just dst -> go dst steps

isCapture :: Move -> Bool
isCapture move = any sCapture (moveSteps move)

capturesCount :: Move -> Int
capturesCount move = length $ filter sCapture (moveSteps move)

applyStep :: Piece -> Address -> Step -> Board -> (Board, Address)
applyStep piece@(Piece _ side) src step board =
  case checkWellFormedStep side board src step of
    Nothing -> error $ "Step is not well-formed: " ++ show step
    Just dst ->
        if sPromote step
          then (setPiece dst (Piece King side) board, dst)
          else (setPiece dst piece board, dst)

applyMove :: Side -> Move -> Board -> Board
applyMove side move board = go board (moveBegin move) (moveSteps move)
  where
    go b _ [] = b
    go b src (step : steps) =
      let (b', dst) = applyStep piece src step b
      in  go b' dst steps

    piece = fromJust (getPiece (moveBegin move) board)

moveEnd :: Side -> Board -> Move -> Address
moveEnd side board move = last $ allPassedAddresses side board move

simpleMove :: Side -> Address -> PlayerDirection -> Move
simpleMove side src dir = Move src [Step dir False promote]
  where
    promote = case neighbour (myDirection side dir) src of
                Nothing -> False
                Just dst -> let last = isLastHorizontal side dst
                            in trace (show dst ++ " is last: " ++ show last) last

simpleCapture :: Side -> Address -> PlayerDirection -> Move
simpleCapture side src dir = Move src [Step dir True False, Step dir False promote]
  where
    promote = case neighbour (myDirection side dir) src of
                Nothing -> False
                Just dst -> isLastHorizontal side dst

makeLine :: [String] -> [Address]
makeLine labels = map (\l -> Address l Nothing Nothing Nothing Nothing) labels

line1 :: [Address]
line1 = makeLine ["a1", "c1", "e1", "g1"]

line2 :: [Address]
line2 = makeLine ["b2", "d2", "f2", "h2"]

line3 :: [Address]
line3 = makeLine ["a3", "c3", "e3", "g3"]

line4 :: [Address]
line4 = makeLine ["b4", "d4", "f4", "h4"]

line5 :: [Address]
line5 = makeLine ["a5", "c5", "e5", "g5"]

line6 :: [Address]
line6 = makeLine ["b6", "d6", "f6", "h6"]

line7 :: [Address]
line7 = makeLine ["a7", "c7", "e7", "g7"]

line8 :: [Address]
line8 = makeLine ["b8", "d8", "f8", "h8"]

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

--   2   2   2
--  / \ / \ /
-- 1   1   1
linkLines1 :: [Address] -> [Address] -> ([Address], [Address])
linkLines1 l1 l2 = unzip $ unstitch $ stitch True $ mix l1 l2
  where

    mix [] [] = []
    mix (a:as) (b:bs) = a : b : mix as bs

    stitch flag [a, b] =
      let (a', b') = linkA flag a b
      in [a', b']
    stitch flag (a1 : as) =
      let (a2 : as') = stitch (not flag) as
          (a1', a2') = linkA flag a1 a2
      in  a1' : a2' : as'

    unstitch [] = []
    unstitch (a1 : a2 : as) = (a1, a2) : unstitch as

-- 2   2   2
--  \ / \ / \
--   1   1   1
linkLines2 :: [Address] -> [Address] -> ([Address], [Address])
linkLines2 l1 l2 = unzip $ unstitch $ stitch False $ mix l1 l2
  where

    mix [] [] = []
    mix (a:as) (b:bs) = b : a : mix as bs

    stitch flag [b, a] =
      let (b', a') = linkA flag b a
      in [b', a']
    stitch flag (a1 : as) =
      let (a2 : as') = stitch (not flag) as
          (a1', a2') = linkA flag a1 a2
      in  a1' : a2' : as'

    unstitch [] = []
    unstitch (a1 : a2 : as) = (a2, a1) : unstitch as

-- linkLines2 l1 l2 = unstitch $ stitch False l1 l2
--   where
--     stitch flag [a] [b] =
--       let (a', b') = linkA flag a b
--       in [(a', True), (b', False)]
--     stitch True (a:as) (b:bs) =
--       let (a', b') = linkPrimary a b
--       in (a', True) : stitch False as (b':bs)
--     stitch False (a:as) (b:bs) =
--       let (a', b') = linkSecondary a b
--       in (b', False) : stitch True (a':as) bs
-- 
--     unstitch pairs =
--       let (as, bs) = partition snd pairs
--       in  (map fst as, map fst bs)

-- linkLines2 l1 l2 =
--   let (l1', l2') = unzip $ zipWith linkSecondary l1 l2
--       (l1'', l2'') = unzip $ zipWith linkPrimary (init l1') (tail l2')
--   in  (l1'' ++ [last l1'], head l2' : l2'')

linkLines :: Bool -> [Address] -> [Address] -> ([Address], [Address])
linkLines True = linkLines1
linkLines False = linkLines2

linkBoard :: [[Address]] -> [[Address]]
linkBoard lines = go True lines
  where
    go _ [] = []
    go flag [l1, l2] =
      let (l1', l2') = linkLines flag l1 l2
      in  [l1', l2']
    go flag (l1 : ls) =
      let (l2 : ls') = go (not flag) ls
          (l1', l2') = linkLines flag l1 l2
      in  l1' : l2' : ls'

buildBoard :: [[Address]] -> Board
buildBoard lines =
  let lines' = linkBoard lines
      addresses = M.fromList [(aLabel a, a) | a <- concat lines']
  in  Board M.empty addresses

resolve :: String -> Board -> Address
resolve label board = fromMaybe (error $ "resolve: unknown field: " ++ label) $ M.lookup label (bAddresses board)

getPiece :: Address -> Board -> Maybe Piece
getPiece a b = M.lookup a (bPieces b)

getPiece' :: String -> Board -> Maybe Piece
getPiece' l b = M.lookup a (bPieces b)
  where
    a = fromMaybe (error $ "getPiece': unknown field: " ++ l) $ M.lookup l (bAddresses b)

setPiece :: Address -> Piece -> Board -> Board
setPiece a p b = b {bPieces = M.insert a p (bPieces b)}

removePiece :: Address -> Board -> Board
removePiece a b = b {bPieces = M.delete a (bPieces b)}

movePiece :: Address -> Address -> Board -> Board
movePiece src dst board =
  case getPiece src board of
    Nothing -> error $ "movePiece: no piece at " ++ show src
    Just piece -> setPiece dst piece $ removePiece src board

setPiece' :: String -> Piece -> Board -> Board
setPiece' l p b = b {bPieces = M.insert a p (bPieces b)}
  where
    a = fromMaybe (error $ "setPiece': unknown field: " ++ l) $ M.lookup l (bAddresses b)

setManyPieces :: [Address] -> Piece -> Board -> Board
setManyPieces addresses piece board = foldr (\a b -> setPiece a piece b) board addresses

board8 :: Board
board8 =
  let board = buildBoard [line1, line2, line3, line4, line5, line6, line7, line8]
      labels1 = line1 ++ line2 ++ line3
      labels2 = line8 ++ line7 ++ line6
  in  setManyPieces labels1 (Piece Man First) $ setManyPieces labels2 (Piece Man Second) board

