{-# LANGUAGE DeriveDataTypeable #-}
module Rules.Russian (
        Russian (..),
        Capture (..),
        translateCapture,
        possibleSimpleMoves1,
        kingCaptures1,
        kingCaptures
      ) where

import Data.Typeable
import Data.List

import Core.Types
import Core.Board

-- import Debug.Trace

data Russian = Russian
  deriving (Show, Eq, Ord, Typeable)

instance GameRules Russian where
  initBoard Russian = board8

  boardSize Russian = (8, 8)

  boardNotation Russian = chessNotation

  parseNotation Russian = parseChessNotation

  rulesName Russian = "russian"

  possibleMoves Russian side board =
    let simpleMoves = concatMap (possibleSimpleMoves1 Russian board) (allMyAddresses side board)
        captures = concatMap (possibleCaptures1 Russian board) (allMyAddresses side board)
    in  if null captures
          then simpleMoves
          else captures
--                let captures' = sortOn (negate . capturesCount) captures
--                    n = capturesCount (head captures')
--                in  filter (\c -> capturesCount c == n) captures'

  updateRules Russian _ = Russian

  getGameResult = genericGameResult

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

possibleSimpleMoves1 :: GameRules rules => rules -> Board -> Address -> [Move]
possibleSimpleMoves1 rules board src =
  case getPiece src board of
    Nothing -> error $ "possibleSimpleMoves1: not my field"
    Just piece@(Piece Man _) -> manSimpleMoves rules piece board src
    Just piece@(Piece King _) -> kingSimpleMoves rules piece board src

possibleCaptures1 :: GameRules rules => rules -> Board -> Address -> [Move]
possibleCaptures1 rules board src =
  case getPiece src board of
    Nothing -> error $ "possibleCaptures: not my field"
    Just piece@(Piece Man _) -> manCaptures rules Nothing piece board src
    Just piece@(Piece King _) -> kingCaptures rules Nothing piece board src

possibleMoves1 :: GameRules rules => rules -> Side -> Board -> Address -> [Move]
possibleMoves1 rules side board src =
  case getPiece src board of
    Nothing -> error $ "possibleMoves1: not my field"
    Just piece@(Piece Man _) -> manMoves rules piece board src
    Just piece@(Piece King _) -> kingMoves rules piece board src

manMoves :: GameRules rules => rules -> Piece -> Board -> Address -> [Move]
manMoves rules piece@(Piece _ side) board src =
  let captures = manCaptures rules Nothing piece board src
      moves = manSimpleMoves rules piece board src
  in  captures ++ moves

manSimpleMoves :: GameRules rules => rules -> Piece -> Board -> Address -> [Move]
manSimpleMoves rules piece@(Piece _ side) board src = check ForwardLeft ++ check ForwardRight
  where
    check dir =
      case neighbour (myDirection rules side dir) src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [Move src [Step dir False (promote dst)]]
                      else []

    promote addr = isLastHorizontal side addr

captures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
captures1 rules mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures1 rules mbPrevDir piece board src
    (Piece King _) -> kingCaptures1 rules mbPrevDir piece board src

pieceCaptures :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
pieceCaptures rules mbPrevDir piece board src =
  case piece of
    (Piece Man _) -> manCaptures rules mbPrevDir piece board src
    (Piece King _) -> kingCaptures rules mbPrevDir piece board src

manCaptures :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures rules mbPrevDir piece@(Piece _ side) board src =
  let captures = captures1 rules mbPrevDir piece board src
      nextMoves m = pieceCaptures rules (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove rules side m board
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture side board capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

manCaptures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
manCaptures1 rules mbPrevDir piece@(Piece _ side) board src = concatMap (check src) $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where

    allowedDir dir =
      case mbPrevDir of
        Nothing -> True
        Just prevDir -> oppositeDirection prevDir /= dir

    check a dir =
      case neighbour (myDirection rules side dir) a of
        Nothing -> []
        Just victimAddr ->
          case getPiece victimAddr board of
            Nothing -> []
            Just victim ->
              if isMyPiece side victim
                then []
                else case neighbour (myDirection rules side dir) victimAddr of
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

kingCaptures :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
kingCaptures rules mbPrevDir piece@(Piece _ side) board src =
  let captures = captures1 rules mbPrevDir piece board src
      nextMoves m = pieceCaptures rules (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove rules side m board
  in nub $ concat $ flip map captures $ \capture1 ->
            let moves1 = translateCapture side board capture1
                allNext = map nextMoves moves1
                isLast = all null allNext
            in  if isLast
                  then moves1
                  else [catMoves move1 move2 | move1 <- moves1, move2 <- nextMoves move1]

kingCaptures1 :: GameRules rules => rules -> Maybe PlayerDirection -> Piece -> Board -> Address -> [Capture]
kingCaptures1 rules mbPrevDir piece@(Piece _ side) board src = concatMap check $ filter allowedDir [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
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
      case neighbour (myDirection rules side dir) a of
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
      case neighbour (myDirection rules side dir) addr of
        Nothing -> []
        Just a' -> if isFree a' board
                     then a' : freeFields dir a'
                     else []

kingSimpleMoves :: GameRules rules => rules -> Piece -> Board -> Address -> [Move]
kingSimpleMoves rules piece@(Piece _ side) board src =
    concatMap check [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
  where
    check dir =
      let free = findFree src dir
      in  [Move src (replicate n (Step dir False False)) | n <- [1..free]]

    findFree a dir =
      case neighbour (myDirection rules side dir) a of
        Nothing -> 0
        Just a' -> if isFree a' board
                     then 1 + findFree a' dir
                     else 0

kingMoves :: GameRules rules => rules -> Piece -> Board -> Address -> [Move]
kingMoves rules piece@(Piece _ side) board src =
  kingCaptures rules Nothing piece board src ++ kingSimpleMoves rules piece board src

