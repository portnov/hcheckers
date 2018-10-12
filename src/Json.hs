{-# LANGUAGE OverloadedStrings #-}
module Json where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Data.List
import qualified Data.Map as M
import Data.Aeson
import Data.Aeson.Types
import Text.Printf
import qualified Data.Text as T

import Types
import Game
import Supervisor

instance ToJSON PlayerDirection where

instance ToJSON PieceKind

instance FromJSON PieceKind

instance ToJSON Side

instance FromJSON Side

instance ToJSON Step where
  toJSON (Step direction capture promote) =
    object ["direction" .= direction, "capture" .= capture, "promote" .= promote]

instance ToJSON StepRep where
  toJSON (StepRep field capture promote) =
    object ["field" .= field, "capture" .= capture, "promote" .= promote]

instance ToJSON MoveRep where
  toJSON (ShortMoveRep from to) = object ["from" .= from, "to" .= to]
  toJSON (FullMoveRep from steps) =
    object ["from" .= from, "steps" .= steps]

instance FromJSON MoveRep where
  parseJSON (Object v) = ShortMoveRep
    <$> v .: "from"
    <*> v .: "to"
  parseJSON invalid = typeMismatch "MoveRep" invalid

instance ToJSON Piece where
  toJSON (Piece kind side) = object ["kind" .= kind, "side" .= side]

instance FromJSON Piece where
  parseJSON (Object v) = Piece
    <$> v .: "kind"
    <*> v .: "side"
  parseJSON invalid = typeMismatch "Piece" invalid

instance ToJSON BoardRep where
  toJSON (BoardRep list) = toJSON list

instance FromJSON BoardRep where
  parseJSON v = BoardRep <$> parseJSON v

instance ToJSON ThreadId where
  toJSON id = toJSON (show id)

-- instance FromJSON ThreadId where
--   parseJSON (String text) = return $ read $ T.unpack text
--   parseJSON invalid = typeMismatch "ThreadId" invalid

-- instance ToJSON Player

-- instance FromJSON Player

instance FromJSON NewGameRq where
  parseJSON = withObject "NewGame" $ \v -> NewGameRq
    <$> v .: "rules"
    <*> v .:? "params" .!= Null
    <*> v .:? "board"

instance FromJSON AttachAiRq where
  parseJSON = withObject "AttachAi" $ \v -> AttachAiRq
    <$> v .: "ai"
    <*> v .:? "params" .!= Null

instance ToJSON Notify where
  toJSON (Notify to from move board) =
    object ["to_side" .= to, "from_side" .= from, "move" .= move, "board" .= board]

instance ToJSON RsPayload where
  toJSON (NewGameRs id) = object ["id" .= id]
  toJSON RegisterUserRs = object ["register_user" .= ("ok" :: T.Text)]
  toJSON AttachAiRs = object ["attach_ai" .= ("ok" :: T.Text)]
  toJSON RunGameRs = object ["run_game" .= ("ok" :: T.Text)]
  toJSON (PollRs messages) = toJSON messages
  toJSON (StateRs board side) = object ["board" .= board, "side" .= side]
  toJSON (PossibleMovesRs moves) = toJSON moves
  toJSON (MoveRs board) = toJSON board

instance ToJSON SupervisorRs where
  toJSON (SupervisorRs payload messages) = object ["response" .= payload, "messages" .= messages]

