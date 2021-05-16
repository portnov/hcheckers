{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Rules.Czech (Czech, czech) where

import Data.Typeable

import Core.Types
import Core.Board
import Core.Evaluator
import Rules.Generic
import Rules.Russian as Russian

newtype Czech = Czech GenericRules
  deriving (Typeable, HasBoardOrientation)

instance Show Czech where
  show = rulesName

instance HasTopology Czech where
  boardTopology _ = Core.Types.Diagonal

instance HasSideNotation Czech where
  sideNotation r = chessSideNotation (boardSize r)

instance HasBoardSize Czech where
  boardSize (Czech r) = boardSize r

instance GameRules Czech where
  type EvaluatorForRules Czech = SimpleEvaluator
  pdnId _ = "29"
  rulesName _ = "czech"

  initBoard rnd _ = initBoard rnd Russian.russian

  initPiecesCount _ = 24

  dfltEvaluator r = defaultEvaluator r

  boardNotation _ = boardNotation russian

  parseNotation _ = parseNotation russian

  possibleMoves (Czech rules) side board = gPossibleMoves rules side board
  mobilityScore (Czech rules) side board = gMobilityScore rules side board

  updateRules r _ = r

  getGameResult = genericGameResult

manCaptures :: GenericRules -> CaptureState -> [PossibleMove]
manCaptures rules ct@(CaptureState {..}) =
  let side = pieceSide ctPiece
      captures = gPieceCaptures1 rules ct
      -- when last horizontal reached, pass non-promoted piece
      -- to next moves check; man cant capture backwards, so
      -- the piece will stop there.
      nextMoves pm = genericNextMoves rules ct False pm
  in concat $ flip map captures $ \capture ->
       let [move1] = translateCapture ctPiece capture
           moves2 = nextMoves move1
       in  if null moves2
             then [move1]
             else [catPMoves move1 move2 | move2 <- moves2]

selectMoves :: GenericRules -> Side -> Board -> MoveDecisionInput -> [PossibleMove]
selectMoves _ side board (MoveDecisionInput {..}) =
    if mdiHasKingCaptures
      then mdiKingCaptures
      else if mdiHasMenCaptures
             then mdiMenCaptures
             else mdiKingSimpleMoves ++ mdiMenSimpleMoves

czechBase :: GenericRules -> GenericRules
czechBase =
  let rules this = (abstractRules this) {
                      gManCaptureDirections = [ForwardLeft, ForwardRight],
                      gSelectMoves = selectMoves this,
                      gManCaptures = manCaptures this
                    }
  in rules

czech :: Czech
czech = Czech $
  let rules = czechBase rules
  in  rules

