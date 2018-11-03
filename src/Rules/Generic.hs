{-# LANGUAGE DeriveDataTypeable #-}
module Rules.Generic where

import Data.Typeable
import Data.List
import Text.Printf

import Core.Types
import Core.Board
import Core.BoardMap
import Core.Evaluator

data Capture = Capture {
    cSrc :: Address,
    cDirection :: PlayerDirection,
    cInitSteps :: Int,
    cVictim :: Address,
    cFreeSteps :: Int,
    cDst :: Address,
    cPromote :: Bool
  }

data GenericRules = GenericRules {
    gPossibleMoves :: Side -> Board -> [PossibleMove]
  , gPossibleSimpleMoves1 :: Board -> Address -> [PossibleMove]
  , gPossibleCaptures1 :: Board -> Address -> [PossibleMove]
  , gManSimpleMoves :: Piece -> Board -> Address -> [PossibleMove]
  , gKingSimpleMoves :: Piece -> Board -> Address -> [PossibleMove]
  , gManCaptures :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove]
  , gKingCaptures ::  Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove]
  , gPieceCaptures1 :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [Capture] 
  , gPieceCaptures :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [PossibleMove] 
  , gManCaptures1 :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [Capture]
  , gKingCaptures1 :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> [Capture]
  , gCanCaptureFrom :: Maybe PlayerDirection -> LabelSet -> Piece -> Board -> Address -> Bool
  , gManSimpleMoveDirections :: [PlayerDirection]
  , gKingSimpleMoveDirections :: [PlayerDirection]
  , gManCaptureDirections :: [PlayerDirection]
  , gKingCaptureDirections :: [PlayerDirection]
  , gBoardOrientation :: BoardOrientation
  , gCaptureMax :: Bool
  }

instance HasBoardOrientation GenericRules where
  boardOrientation = gBoardOrientation

translateCapture :: Piece -> Capture -> [PossibleMove]
translateCapture piece@(Piece _ side) capture =
    [PossibleMove {
      pmBegin = src,
      pmEnd = dst,
      pmVictims = [victim],
      pmMove = Move src (steps n),
      pmPromote = promote,
      pmResult = [Take src, RemoveCaptured victim, Put dst piece']
    } | n <- [1 .. cFreeSteps capture]]
  where
    steps n = replicate (cInitSteps capture) (Step dir False False) ++
              [Step dir True False] ++
              replicate (n-1) (Step dir False False) ++
              [Step dir False promote]
    dir = cDirection capture
    promote = cPromote capture
    src = cSrc capture
    dst = cDst capture
    victim = cVictim capture
    piece' = if promote then promotePiece piece else piece


freeFields :: HasBoardOrientation rules => rules -> Side -> PlayerDirection -> Address -> Board -> (Int, [Address])
freeFields rules side dir addr board =
  case myNeighbour rules side dir addr of
    Nothing -> (0, [])
    Just a' -> if isFree a' board
                 then let (n, prev) = freeFields rules side dir a' board
                      in  (n+1, a' : prev)
                 else (0, [])

abstractRules :: GenericRules -> GenericRules
abstractRules =
  let
    possibleMoves rules side board =
      let simpleMoves = concatMap (gPossibleSimpleMoves1 rules board) (allMyAddresses side board)
          captures = concatMap (gPossibleCaptures1 rules board) (allMyAddresses side board)
      in  if gCaptureMax rules
            then let captures' = sortOn (negate . length . pmVictims) captures
                     n = length $ pmVictims (head captures')
                 in  filter (\c -> length (pmVictims c) == n) captures'
            else if null captures
                   then simpleMoves
                   else captures

    possibleSimpleMoves1 rules board src =
      case getPiece src board of
        Nothing -> error $ "possibleSimpleMoves1: not my field"
        Just piece@(Piece Man _) -> gManSimpleMoves rules piece board src
        Just piece@(Piece King _) -> gKingSimpleMoves rules piece board src

    possibleCaptures1 rules board src =
      case getPiece src board of
        Nothing -> error $ "possibleCaptures: not my field"
        Just piece@(Piece Man _) -> gManCaptures rules Nothing emptyLabelSet piece board src
        Just piece@(Piece King _) -> gKingCaptures rules Nothing emptyLabelSet piece board src

    pieceCaptures rules mbPrevDir captured piece board src =
      case piece of
        (Piece Man _) -> gManCaptures rules mbPrevDir captured piece board src
        (Piece King _) -> gKingCaptures rules mbPrevDir captured piece board src

    manSimpleMoves rules piece@(Piece _ side) board src =
        concatMap check (gManSimpleMoveDirections rules)
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> []
            Just dst -> if isFree dst board
                          then let move = Move src [Step dir False promote]
                                   promote = isLastHorizontal side dst
                                   piece' = if promote then promotePiece piece else piece
                               in  [PossibleMove {
                                     pmBegin = src,
                                     pmEnd = dst,
                                     pmVictims = [],
                                     pmMove = move,
                                     pmPromote = promote,
                                     pmResult = [Take src, Put dst piece']
                                    }]
                          
                          else []

    pieceCaptures1 rules mbPrevDir captured piece board src =
      case piece of
        (Piece Man _) -> gManCaptures1 rules mbPrevDir captured piece board src
        (Piece King _) -> gKingCaptures1 rules mbPrevDir captured piece board src

    manCaptures1 rules mbPrevDir captured piece@(Piece _ side) board src =
        concatMap (check src) $ filter allowedDir (gManCaptureDirections rules)
      where

        allowedDir dir =
          case mbPrevDir of
            Nothing -> True
            Just prevDir -> oppositeDirection prevDir /= dir

        check a dir =
          case myNeighbour rules side dir a of
            Just victimAddr | not (aLabel victimAddr `labelSetMember` captured) ->
              case getPiece victimAddr board of
                Nothing -> []
                Just victim ->
                  if isMyPiece side victim
                    then []
                    else case myNeighbour rules side dir victimAddr of
                           Nothing -> []
                           Just freeAddr -> if isFree freeAddr board
                                              then [Capture {
                                                      cSrc = a,
                                                      cDirection = dir,
                                                      cInitSteps = 0,
                                                      cVictim = victimAddr,
                                                      cFreeSteps = 1,
                                                      cDst = freeAddr,
                                                      cPromote = isLastHorizontal side freeAddr
                                                    }]
                                              else []
            _ -> []

    -- This is a most popular implementation, which fits most rules
    -- except for english / checkers.
    kingCaptures1 rules mbPrevDir captured piece@(Piece _ side) board src =
        concatMap check $ filter allowedDir (gKingCaptureDirections rules)
      where
        
        allowedDir dir =
          case mbPrevDir of
            Nothing -> True
            Just prevDir -> oppositeDirection prevDir /= dir

        check dir =
          case search dir src of
            Nothing -> []
            Just (victimAddr, initSteps) ->
              case freeFields rules side dir victimAddr board of
                (0,_) -> []
                (nFree, fields) -> 
                    [mkCapture dir initSteps victimAddr freeSteps (fields !! (freeSteps-1)) | freeSteps <- [1 .. nFree]]

        mkCapture dir init victim free dst =
          Capture {
            cSrc = src,
            cDirection = dir,
            cInitSteps = init,
            cVictim = victim,
            cFreeSteps = free,
            cDst = dst,
            cPromote = False
          }

        search :: PlayerDirection -> Address -> Maybe (Address, Int)
        search dir a =
          case myNeighbour rules side dir a of
            Nothing -> Nothing
            Just a' -> case getPiece a' board of
                         Nothing -> case search dir a' of
                                      Nothing -> Nothing
                                      Just (victimAddr, steps) -> Just (victimAddr, steps + 1)
                         Just p -> if isOpponentPiece side p && not (aLabel a' `labelSetMember` captured)
                                     then Just (a', 0)
                                     else Nothing

    -- This is most popular implementation, which fits most rules
    -- except for english / checkers
    kingCaptures rules mbPrevDir captured piece@(Piece _ side) board src =
      let captures = gPieceCaptures1 rules mbPrevDir captured piece board src
          grouped = groupBy (\c1 c2 -> cDirection c1 == cDirection c2) $ sortOn cDirection captures
          capturesByDirection = [(cDirection (head cs), cs) | cs <- grouped]
          nextMoves pm = gPieceCaptures rules (Just $ firstMoveDirection m) captured' piece b (pmEnd pm)
                           where
                            m = pmMove pm
                            b = setPiece (pmEnd pm) piece board
                            captured' = foldr insertLabelSet captured (map aLabel $ pmVictims pm)
      in nub $ concat $ flip map capturesByDirection $ \(dir, captures) ->
                let moves1 c = translateCapture piece c
                    allNext c = map nextMoves (moves1 c)
                    isLast c = all null (allNext c)
                in  if all isLast captures
                      then concatMap moves1 captures
                      else [catPMoves move1 move2 | c <- captures, move1 <- moves1 c, move2 <- nextMoves move1]

    -- This is most popular implementation, which fits most rules
    -- except for english / checkers
    kingSimpleMoves rules piece@(Piece _ side) board src =
        concatMap check (gKingSimpleMoveDirections rules)
      where
        check dir =
          let (nFree,free) = freeFields rules side dir src board
          in [PossibleMove {
                pmBegin = src,
                pmEnd = free !! (n-1),
                pmVictims = [],
                pmMove = Move src (replicate n (Step dir False False)),
                pmPromote = False,
                pmResult = [Take src, Put (free !! (n-1)) piece]
              } | n <- [1..nFree]]

    canCaptureFrom rules mbPrevDir captured piece@(Piece Man side) board src =
        null (gManCaptures1 rules mbPrevDir captured piece board src)
    canCaptureFrom rules mbPrevDir captured piece@(Piece King side) board src =
        null (gKingCaptures1 rules mbPrevDir captured piece board src)

    manSimpleMoveDirections rules = [ForwardLeft, ForwardRight]

    orientation rules = FirstAtBottom

    rules this = GenericRules {
      gPossibleMoves = possibleMoves this
    , gPossibleSimpleMoves1 = possibleSimpleMoves1 this
    , gPossibleCaptures1 = possibleCaptures1 this
    , gManSimpleMoves = manSimpleMoves this
    , gKingSimpleMoves = kingSimpleMoves this
    , gKingCaptures1 = kingCaptures1 this
    , gKingCaptures = kingCaptures this
    , gPieceCaptures1 = pieceCaptures1 this
    , gPieceCaptures = pieceCaptures this
    , gManCaptures1 = manCaptures1 this
    , gManCaptures = error "gManCaptures has to be implemented in specific rules"
    , gCanCaptureFrom = canCaptureFrom this
    , gManSimpleMoveDirections = manSimpleMoveDirections this
    , gKingSimpleMoveDirections =  [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
    , gManCaptureDirections = [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
    , gKingCaptureDirections =  [ForwardLeft, ForwardRight, BackwardLeft, BackwardRight]
    , gBoardOrientation = orientation this
    , gCaptureMax = False
    }
  in rules

