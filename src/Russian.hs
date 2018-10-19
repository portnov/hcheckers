{-# LANGUAGE DeriveDataTypeable #-}
module Russian where

import Control.Monad
import Control.Monad.State
import Data.Maybe
import qualified Data.Map as M
import Data.Typeable
import Data.Ord
import Data.List
import Text.Printf

import Types
import Board

import Debug.Trace

data Russian = Russian
  deriving (Show, Eq, Ord, Typeable)

instance GameRules Russian where
  initBoard Russian = board8

  rulesName Russian = "russian"

  possibleMoves Russian side board =
    let simpleMoves = concatMap (possibleSimpleMoves1 board) (allMyAddresses side board)
        captures = concatMap (possibleCaptures1 board) (allMyAddresses side board)
    in  if null captures
          then simpleMoves
          else captures
--                let captures' = sortOn (negate . capturesCount) captures
--                    n = capturesCount (head captures')
--                in  filter (\c -> capturesCount c == n) captures'

  updateRules Russian _ = Russian

data Capture = Capture {
    cSrc :: Address,
    cDirection :: PlayerDirection,
    cInitSteps :: Int,
    cFreeSteps :: Int,
--     cDst :: Address,
    cPromote :: Bool
  }

translateCapture :: Side -> Board -> Capture -> [Move]
translateCapture side board capture =
    [Move src (steps n) | n <- [1 .. cFreeSteps capture]]
  where
    src = cSrc capture
    dir = cDirection capture
    promote = cPromote capture
    steps n = replicate (cInitSteps capture) (Step dir False False) ++
              [Step dir True False] ++
              replicate (n-1) (Step dir False False) ++
              [Step dir False promote]

possibleSimpleMoves1 :: Board -> Address -> [Move]
possibleSimpleMoves1 board src =
  case getPiece src board of
    Nothing -> error $ "possibleSimpleMoves1: not my field"
    Just piece@(Piece Man _) -> manSimpleMoves piece board src
    Just piece@(Piece King _) -> kingSimpleMoves piece board src

possibleCaptures1 :: Board -> Address -> [Move]
possibleCaptures1 board src =
  case getPiece src board of
    Nothing -> error $ "possibleCaptures: not my field"
    Just piece@(Piece Man _) -> manCaptures Nothing piece board src
    Just piece@(Piece King _) -> kingCaptures Nothing piece board src

possibleMoves1 :: Side -> Board -> Address -> [Move]
possibleMoves1 side board src =
  case getPiece src board of
    Nothing -> error $ "possibleMoves1: not my field"
    Just piece@(Piece Man _) -> manMoves piece board src
    Just piece@(Piece King _) -> kingMoves piece board src

manMoves :: Piece -> Board -> Address -> [Move]
manMoves piece@(Piece _ side) board src =
  let captures = manCaptures Nothing piece board src
      moves = manSimpleMoves piece board src
  in  captures ++ moves

manSimpleMoves :: Piece -> Board -> Address -> [Move]
manSimpleMoves piece@(Piece _ side) board src = check ForwardLeft ++ check ForwardRight
  where
    check dir =
      case neighbour (myDirection side dir) src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [Move src [Step dir False (promote dst)]]
                      else []

    promote addr = isLastHorizontal side addr

captures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
captures1 mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures1 mbPrevDir piece board src
    (Piece King _) -> kingCaptures1 mbPrevDir piece board src

pieceCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
pieceCaptures mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures mbPrevDir piece board src
    (Piece King _) -> kingCaptures mbPrevDir piece board src

manCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures mbPrevDir piece@(Piece _ side) board src =
  let captures = captures1 mbPrevDir piece board src
      nextMoves m = pieceCaptures (Just $ captureDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture side board capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

captureDirection :: Move -> PlayerDirection
captureDirection move = sDirection $ head $ moveSteps move

manCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
manCaptures1 mbPrevDir piece@(Piece _ side) board src = concatMap (check src) $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection side dir) a of
        Nothing -> []
        Just victimAddr ->
          case getPiece victimAddr board of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection side dir) victimAddr of
                       Nothing -> []
                       Just freeAddr -> if isFree freeAddr board
                                          then [Capture {
                                                  cSrc = a,
                                                  cDirection = dir,
                                                  cInitSteps = 0,
                                                  cFreeSteps = 1,
                                                  -- cDst = freeAddr,
                                                  cPromote = isLastHorizontal side freeAddr
                                                }]
                                          else []

kingCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
kingCaptures mbPrevDir piece@(Piece _ side) board src =
  let captures = captures1 mbPrevDir piece board src
      nextMoves m = pieceCaptures (Just $ captureDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = translateCapture side board capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

kingCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
kingCaptures1 mbPrevDir piece@(Piece _ side) board src = concatMap check $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    
    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check dir =
      case search dir src of
        Nothing -> []
        Just (victimAddr, initSteps) ->
          case freeFields dir victimAddr of
            [] -> []
            fields -> 
                [mkCapture dir initSteps freeSteps (fields !! (freeSteps-1)) | freeSteps <- [1 .. length fields]]

    mkCapture dir init free dst =
      Capture {
        cSrc = src,
        cDirection = dir,
        cInitSteps = init,
        cFreeSteps = free,
        -- cDst = dst,
        cPromote = False
      }

    search :: PlayerDirection -> Address -> Maybe (Address, Int)
    search dir a =
      case neighbour (myDirection side dir) a of
        Nothing -> Nothing
        Just a' -> case getPiece a' board of
                     Nothing -> case search dir a' of
                                  Nothing -> Nothing
                                  Just (victimAddr, steps) -> Just (victimAddr, steps + 1)
                     Just p -> if isOpponentPiece side p
                                 then Just (a', 0)
                                 else Nothing

    freeFields :: PlayerDirection -> Address -> [Address]
    freeFields dir addr =
      case neighbour (myDirection side dir) addr of
        Nothing -> []
        Just a' -> if isFree a' board
                     then a' : freeFields dir a'
                     else []

kingSimpleMoves :: Piece -> Board -> Address -> [Move]
kingSimpleMoves piece@(Piece _ side) board src =
    concatMap check [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    check dir =
      let free = findFree src dir
      in  [Move src (replicate n (Step dir False False)) | n <- [1..free]]

    findFree a dir =
      case neighbour (myDirection side dir) a of
        Nothing -> 0
        Just a' -> if isFree a' board
                     then 1 + findFree a' dir
                     else 0

kingMoves :: Piece -> Board -> Address -> [Move]
kingMoves piece@(Piece _ side) board src =
  kingCaptures Nothing piece board src ++ kingSimpleMoves piece board src

