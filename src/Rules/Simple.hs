{-# LANGUAGE DeriveDataTypeable #-}
module Rules.Simple (Simple (..)) where

import Data.Typeable

import Core.Types
import Core.Board
import qualified Rules.Russian as Russian

data Simple = Simple
  deriving (Show, Eq, Ord, Typeable)

instance GameRules Simple where
  initBoard Simple = initBoard Russian.Russian
  boardSize Simple = boardSize Russian.Russian

  boardNotation Simple = boardNotation Russian.Russian

  parseNotation Simple = parseNotation Russian.Russian

  rulesName Simple = "simple"

  updateRules Simple _ = Simple

  getGameResult = genericGameResult

  possibleMoves Simple side board =
    let simpleMoves = concatMap (possibleSimpleMoves1 board) (allMyAddresses side board)
        captures = concatMap (possibleCaptures1 board) (allMyAddresses side board)
    in  if null captures
          then simpleMoves
          else captures

possibleSimpleMoves1 :: Board -> Address -> [Move]
possibleSimpleMoves1 board src =
  case getPiece src board of
    Nothing -> error "possibleSimpleMoves1: not my field"
    Just piece@(Piece Man _) -> manSimpleMoves piece board src
    Just _ -> error "possibleSimpleMoves1: kings are not possible in simple draughts"

manSimpleMoves :: Piece -> Board -> Address -> [Move]
manSimpleMoves piece@(Piece _ side) board src = check ForwardLeft ++ check ForwardRight
  where
    check dir =
      case neighbour (myDirection side dir) src of
        Nothing -> []
        Just dst -> if isFree dst board
                      then [Move src [Step dir False False]]
                      else []

possibleCaptures1 :: Board -> Address -> [Move]
possibleCaptures1 board src =
  case getPiece src board of
    Nothing -> error "possibleCaptures: not my field"
    Just piece@(Piece Man _) -> manCaptures Nothing piece board src
    Just _ -> error "possibleCaptures1: kings are not possible in simple draughts"

manCaptures :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Move]
manCaptures mbPrevDir piece@(Piece _ side) board src =
  let captures = manCaptures1 mbPrevDir piece board src
      nextMoves m = manCaptures (Just $ firstMoveDirection m) p b a
                      where (b, a, p) = applyMove side m board
  in concat $ flip map captures $ \capture ->
       let [move1] = Russian.translateCapture side board capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catMoves move1 move2 | move2 <- moves2]

manCaptures1 :: Maybe PlayerDirection -> Piece -> Board -> Address -> [Russian.Capture]
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
                       Just freeAddr ->
                        if isFree freeAddr board
                          then [Russian.Capture {
                                  Russian.cSrc = a,
                                  Russian.cDirection = dir,
                                  Russian.cInitSteps = 0,
                                  Russian.cFreeSteps = 1,
                                  Russian.cPromote = False
                                }]
                          else []

