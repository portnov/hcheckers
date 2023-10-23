{-# LANGUAGE DeriveGeneric #-}
module Rest.Types where

import qualified Data.Text as T
import Data.Aeson
import GHC.Generics

import Core.Types
import Formats.Types

data BoardRq =
    DefaultBoard
  | ExplicitBoard BoardRep
  | FenBoard T.Text
  | PdnBoard T.Text
  | PrevGameBoard GameId
  | RandomPreset
  deriving (Eq, Show, Generic)

-- | Request for new game creation
data NewGameRq = NewGameRq {
    rqRules :: String         -- ^ Rules identifier
  , rqRulesParams :: Value    -- ^ Rules parameters (no rules support parameters atm)
  , rqTimingControl :: Maybe T.Text -- ^ Name of timing control config
  , rqBoard :: BoardRq
  }
  deriving (Eq, Show, Generic)

-- | Request for attaching AI to the game.
-- Parameter is identifier of AI implementation.
-- Currently there is only one, named 'default'.
data AttachAiRq = AttachAiRq {
    airqImplementation :: T.Text
  , airqPlayerName :: UserName
  , airqParamsFromServer :: Bool
  , airqParams :: Value
  }
  deriving (Eq, Show, Generic)

data PdnInfoRq = PdnInfoRq String T.Text
  deriving (Eq, Show, Generic)

-- | Response to the client.
-- Contains payload and list of notification messages.
data Response = Response RsPayload [Notify]
  deriving (Eq, Show, Generic)

-- | Response payload
data RsPayload =
    NewGameRs GameId Side
  | RegisterUserRs
  | AttachAiRs
  | AttachSpectatorRs
  | RunGameRs
  | RunGameLoopRs
  | PollRs [Notify]
  | LobbyRs [Game]
  | NotationRs BoardSize BoardOrientation [(Label, Notation)] SideNotation
  | TopologyRs BoardTopology
  | StateRs BoardRep GameStatus Side
  | PdnInfoRs PdnInfo
  | HistoryRs [HistoryRecordRep]
  | HistoryBoardRs BoardRep
  | HistoryMoveRs MoveRep BoardRep BoardRep
  | PossibleMovesRs [MoveRep]
  | MoveRs BoardRep (Maybe AiSessionId)
  | PollMoveRs AiSessionStatus
  | AiHintRs Int AiSessionId
  | StopAiRs
  | UndoRs Int BoardRep
  | CapitulateRs
  | DrawRqRs (Maybe AiSessionId)
  | DrawAcceptRs Bool
  | ShutdownRs
  deriving (Eq, Show, Generic)

