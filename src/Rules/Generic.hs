{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
module Rules.Generic where

import Data.List
import Data.Maybe

import Core.Types
import Core.Board
import Core.BoardMap

-- | Describes one jump during capture move;
-- capture can conssit of several jumps.
data Capture = Capture {
      cSrc :: Address                -- ^ Source piece position
    , cDirection :: PlayerDirection  -- ^ Direction of jump
    , cInitSteps :: Int              -- ^ Number of steps by free fields that we have to do before the actual capture.
                                     --   If a piece is a man, this is obviously always 0.
    , cVictim :: Address             -- ^ Position of piece being captured
    , cRemoveVictimImmediately :: Bool
    , cFreeSteps :: Int              -- ^ Number of steps by free fields that we are doing after actual capture.
                                     --   For man, this is always 1.
    , cDst :: Address                -- ^ End position of capture.
    , cPromote :: Bool               -- ^ Whether the piece should be promoted at the end of this jump.
  }

-- | State that we have to track during single capture move.
data CaptureState = CaptureState {
    ctPrevDirection :: Maybe PlayerDirection -- ^ Previous capture direction
  , ctCaptured :: LabelSet                   -- ^ Fields that were already captured; we have to track this to prevent one piece being captured twice
  , ctPiece :: Piece                         -- ^ Piece that is performing the capture
  , ctBoard :: Board                         -- ^ Current board state
  , ctCurrent :: Address                     -- ^ Current position of the piece
  , ctSource :: Address                      -- ^ Starting position of capture
  }

-- | Initial capture state
initState :: Piece -> Board -> Address -> CaptureState
initState piece board src = CaptureState Nothing emptyLabelSet piece board src src

-- | An `Abstract class` for game rules
data GenericRules = GenericRules {
    gPossibleMoves :: Side -> Board -> [PossibleMove]
  , gMobilityScore :: Side -> Board -> Int
  , gPossibleSimpleMoves1 :: Board -> Address -> [PossibleMove]
  , gPossibleCaptures1 :: Board -> Address -> [PossibleMove]
  , gManSimpleMoves :: Side -> Board -> Address -> [PossibleMove]
  , gKingSimpleMoves :: Side -> Board -> Address -> [PossibleMove]
  , gManCaptures :: CaptureState -> [PossibleMove]
  , gKingCaptures ::  CaptureState -> [PossibleMove]
  , gPieceCaptures1 :: CaptureState -> [Capture] 
  , gPieceCaptures :: CaptureState -> [PossibleMove] 
  , gManCaptures1 :: CaptureState -> [Capture]
  , gKingCaptures1 :: CaptureState -> [Capture]
  , gCanCaptureFrom :: CaptureState -> Bool
  , gManSimpleMoveDirections :: [PlayerDirection]
  , gKingSimpleMoveDirections :: [PlayerDirection]
  , gManCaptureDirections :: [PlayerDirection]
  , gKingCaptureDirections :: [PlayerDirection]
  , gBoardOrientation :: BoardOrientation
  , gCaptureMax :: Bool
  , gRemoveCapturedImmediately :: Bool
  }

instance HasBoardOrientation GenericRules where
  boardOrientation = gBoardOrientation

translateCapture :: Piece -> Capture -> [PossibleMove]
translateCapture piece@(Piece _ side) capture =
    [PossibleMove {
      pmBegin = src,
      pmEnd = dst,
      pmVictims = [victim],
      pmMove = Move src (steps $ cFreeSteps capture),
      pmPromote = promote,
      pmResult = [Take src, remove victim, Put dst piece']
    }]
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
    remove = if cRemoveVictimImmediately capture
               then RemoveCaptured
               else MarkCaptured

freeFields :: HasBoardOrientation rules
           => rules
           -> Side
           -> PlayerDirection
           -> LabelSet
           -> Bool
           -> Address
           -> Board
           -> (Int, [Address])
freeFields rules side dir captured allowStepCaptured addr board =
  case myNeighbour rules side dir addr of
    Nothing -> (0, [])
    Just a' -> if isFree a' board || (allowStepCaptured && aLabel a' `labelSetMember` captured)
                 then let (n, prev) = freeFields rules side dir captured allowStepCaptured a' board
                      in  (n+1, a' : prev)
                 else (0, [])

genericNextMoves :: GenericRules -> CaptureState -> Bool -> PossibleMove -> [PossibleMove]
genericNextMoves rules ct@(CaptureState {..}) continuePromoted pm =
    gPieceCaptures rules $ ct {
                             ctPrevDirection = Just (firstMoveDirection m),
                             ctCaptured = captured',
                             ctPiece = piece',
                             ctBoard = b,
                             ctCurrent = pmEnd pm
                           }
  where
    m = pmMove pm
    promoted = if pmPromote pm
                 then promotePiece ctPiece
                 else ctPiece
    piece' = if continuePromoted
               then promoted
               else ctPiece
    b = setPiece (pmEnd pm) piece' $ removePiece ctCurrent ctBoard
    captured' = foldr insertLabelSet ctCaptured (map aLabel $ pmVictims pm)

abstractRules :: GenericRules -> GenericRules
abstractRules =
  let
    possibleMoves rules side board =
      -- let (simpleMoves, captures) = searchMoves rules side board False ([], [])
      let simpleMoves = concatMap (gManSimpleMoves rules side board) (filter (manHasSimpleMoves rules side board) $ myMenA side board) ++
                        concatMap (gKingSimpleMoves rules side board) (myKingsA side board)
          captures = concatMap (manCaptures' rules side board) (filter (manHasCaptures rules side board) $ myMenA side board) ++
                     concatMap (kingCaptures' rules side board) (myKingsA side board)
          -- concatMap (gPossibleCaptures1 rules board) (allMyAddresses side board)
      in  if gCaptureMax rules
            then
              if null captures
                then simpleMoves
                else
                  let captures' = sortOn (negate . length . pmVictims) captures
                      n = length $ pmVictims (head captures')
                  in  filter (\c -> length (pmVictims c) == n) captures'
            else if null captures
                   then simpleMoves
                   else captures

--     searchMoves _ _ _ _ (accSimple, accCaptures) [] = (accSimple, accCaptures)
--     searchMoves rules side board False (accSimple, accCaptures) ((addr, pieceKind) : rest) =
--       case pieceKind of
--         Man | manHasCaptures rules side board addr ->
--                  let captures = manCaptures' rules side board addr
--                  in  searchMoves rules side board True ([], captures ++ accCaptures) rest
--             | manHasSimpleMoves rules side board addr ->
--                  let moves = gManSimpleMoves rules side board addr
--                  in  searchMoves rules side board False (moves ++ accSimple, accCaptures) rest
--             | otherwise ->
--                  searchMoves rules side board False (accSimple, accCaptures) rest
--         King -> let captures = kingCaptures' rules side board addr
--                     simple   = gKingSimpleMoves rules side board addr
--                 in  if null captures
--                       then searchMoves rules side board False (simple ++ accSimple, accCaptures) rest
--                       else searchMoves rules side board True ([], captures ++ accCaptures) rest
--     searchMoves rules side board True (accSimple, accCaptures) ((addr, pieceKind) : rest) =
--       case pieceKind of
--         Man | manHasCaptures rules side board addr ->
--                 let captures = manCaptures' rules side board addr
--                 in  searchMoves rules side board True ([], captures ++ accCaptures) rest
--             | otherwise ->
--                 searchMoves rules side board True (accSimple, accCaptures) rest
--         King -> let captures = kingCaptures' rules side board addr
--                 in  searchMoves rules side board True ([], captures ++ accCaptures) rest

    mobility rules side board =
      let (men, kings) = myCounts side board
      in  if kings > 0
            then kings * 4
            else sum $ map (manSimpleMovesCount rules side board) (myMenA side board)

    manCaptures' rules side board src =
      gManCaptures rules $ initState (Piece Man side) board src

    kingCaptures' rules side board src =
      gKingCaptures rules $ initState (Piece King side) board src

    possibleSimpleMoves1 rules board src =
      case getPiece src board of
        Nothing -> error $ "possibleSimpleMoves1: not my field"
        Just (Piece Man side) -> gManSimpleMoves rules side board src
        Just (Piece King side) -> gKingSimpleMoves rules side board src

    possibleCaptures1 rules board src =
      case getPiece src board of
        Nothing -> error $ "possibleCaptures: not my field"
        Just piece@(Piece Man _) -> gManCaptures rules $ initState piece board src
        Just piece@(Piece King _) -> gKingCaptures rules $ initState piece board src

    pieceCaptures rules ct@(CaptureState {..}) =
      case ctPiece of
        (Piece Man _) -> gManCaptures rules ct
        (Piece King _) -> gKingCaptures rules ct

    manHasSimpleMoves rules side board src = any check (gManSimpleMoveDirections rules)
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> False
            Just dst -> isFree dst board

    manSimpleMovesCount rules side board src = sum $ map check (gManSimpleMoveDirections rules)
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> 0
            Just dst -> if isFree dst board then 1 else 0

    manSimpleMoves rules side board src =
        mapMaybe check (gManSimpleMoveDirections rules)
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> Nothing
            Just dst -> if isFree dst board
                          then let move = Move src [Step dir False promote]
                                   promote = isLastHorizontal side dst
                                   piece' = if promote then Piece King side else Piece Man side
                               in  Just $ PossibleMove {
                                     pmBegin = src,
                                     pmEnd = dst,
                                     pmVictims = [],
                                     pmMove = move,
                                     pmPromote = promote,
                                     pmResult = [Take src, Put dst piece']
                                    }
                          
                          else Nothing

    pieceCaptures1 rules ct@(CaptureState {..}) =
      case ctPiece of
        (Piece Man _) -> gManCaptures1 rules ct
        (Piece King _) -> gKingCaptures1 rules ct

    manHasCaptures rules side board src = any check (gManCaptureDirections rules)
      where
        check dir =
          case myNeighbour rules side dir src of
            Nothing -> False
            Just victimAddr ->
              if isPieceAt victimAddr board (opposite side)
                then case myNeighbour rules side dir victimAddr of
                       Nothing -> False
                       Just dst -> isFree dst board
                else False
    
    allowStepOccupied rules addr captured
      | gRemoveCapturedImmediately rules = aLabel addr `labelSetMember` captured
      | otherwise = False

    canCaptureA rules addr captured =
      not (aLabel addr `labelSetMember` captured)

    manCaptures1 rules (CaptureState {..}) =
        mapMaybe (check ctCurrent) $ filter allowedDir (gManCaptureDirections rules)
      where
        side = pieceSide ctPiece

        allowedDir dir =
          case ctPrevDirection of
            Nothing -> True
            Just prevDir -> oppositeDirection prevDir /= dir

        check a dir =
          case myNeighbour rules side dir a of
            Just victimAddr | not (aLabel victimAddr `labelSetMember` ctCaptured) ->
              if isPieceAt victimAddr ctBoard (opposite side)
                then case myNeighbour rules side dir victimAddr of
                           Nothing -> Nothing
                           Just freeAddr -> if isFree freeAddr ctBoard || allowStepOccupied rules freeAddr ctCaptured
                                              then Just $ Capture {
                                                      cSrc = a,
                                                      cDirection = dir,
                                                      cInitSteps = 0,
                                                      cVictim = victimAddr,
                                                      cRemoveVictimImmediately = gRemoveCapturedImmediately rules,
                                                      cFreeSteps = 1,
                                                      cDst = freeAddr,
                                                      cPromote = isLastHorizontal side freeAddr
                                                    }
                                              else Nothing
                else Nothing
            _ -> Nothing

    -- This is a most popular implementation, which fits most rules
    -- except for english / checkers.
    kingCaptures1 rules (CaptureState {..}) =
        concatMap check $ filter allowedDir (gKingCaptureDirections rules)
      where
        side = pieceSide ctPiece
        
        allowedDir dir =
          case ctPrevDirection of
            Nothing -> True
            Just prevDir -> oppositeDirection prevDir /= dir

        check dir =
          case search dir ctCurrent of
            Nothing -> []
            Just (victimAddr, initSteps) ->
              case freeFields rules side dir ctCaptured (gRemoveCapturedImmediately rules) victimAddr ctBoard of
                (0,_) -> []
                (nFree, fields) -> 
                    [mkCapture dir initSteps victimAddr freeSteps (fields !! (freeSteps-1)) | freeSteps <- [1 .. nFree]]

        mkCapture dir init victim free dst =
          Capture {
            cSrc = ctCurrent,
            cDirection = dir,
            cInitSteps = init,
            cVictim = victim,
            cRemoveVictimImmediately = gRemoveCapturedImmediately rules,
            cFreeSteps = free,
            cDst = dst,
            cPromote = False
          }

        search :: PlayerDirection -> Address -> Maybe (Address, Int)
        search dir a =
          case myNeighbour rules side dir a of
            Nothing -> Nothing
            Just a' -> case getPiece a' ctBoard of
                         Nothing -> case search dir a' of
                                      Nothing -> Nothing
                                      Just (victimAddr, steps) -> Just (victimAddr, steps + 1)
                         Just p -> if isOpponentPiece side p
                                     then if gRemoveCapturedImmediately rules && aLabel a' `labelSetMember` ctCaptured
                                            then case search dir a' of
                                                   Nothing -> Nothing
                                                   Just (victimAddr, steps) -> Just (victimAddr, steps+1)
                                            else Just (a', 0)
                                     else Nothing

    -- This is most popular implementation, which fits most rules
    -- except for english / checkers
    kingCaptures rules ct@(CaptureState {..}) =
      let side = pieceSide ctPiece
          captures = gPieceCaptures1 rules ct
          grouped = groupBy (\c1 c2 -> cDirection c1 == cDirection c2) $ sortOn cDirection captures
          capturesByDirection = [(cDirection (head cs), cs) | cs <- grouped]
          nextMoves pm = genericNextMoves rules ct False pm 
      in nub $ concat $ flip map capturesByDirection $ \(dir, captures) ->
                let moves1 c = translateCapture ctPiece c
                    allNext c = map nextMoves (moves1 c)
                    isLast c = all null (allNext c)
                in  if all isLast captures
                      then concatMap moves1 captures
                      else [catPMoves move1 move2 | c <- captures, move1 <- moves1 c, move2 <- nextMoves move1]

    -- This is most popular implementation, which fits most rules
    -- except for english / checkers
    kingSimpleMoves rules side board src =
        concatMap check (gKingSimpleMoveDirections rules)
      where
        check dir =
          let (nFree,free) = freeFields rules side dir emptyLabelSet False src board
              piece = Piece King side
          in [PossibleMove {
                pmBegin = src,
                pmEnd = free !! (n-1),
                pmVictims = [],
                pmMove = Move src (replicate n (Step dir False False)),
                pmPromote = False,
                pmResult = [Take src, Put (free !! (n-1)) piece]
              } | n <- [1..nFree]]

    canCaptureFrom rules ct@(CaptureState {ctPiece = Piece Man side}) =
        not $ null (gManCaptures1 rules ct)
    canCaptureFrom rules ct@(CaptureState {ctPiece = Piece King side}) =
        not $ null (gKingCaptures1 rules ct)

    manSimpleMoveDirections rules = [ForwardLeft, ForwardRight]

    orientation rules = FirstAtBottom

    rules this = GenericRules {
      gPossibleMoves = possibleMoves this
    , gMobilityScore = mobility this
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
    , gRemoveCapturedImmediately = False
    }
  in rules

