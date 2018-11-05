{-# LANGUAGE OverloadedStrings #-}
module Core.Json where

import Control.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

import Core.Types
import Core.Supervisor

instance ToJSON PlayerDirection where

instance ToJSON PieceKind

instance FromJSON PieceKind

instance ToJSON Side

instance FromJSON Side

instance ToJSON GameResult

instance ToJSON Label where
  toJSON (Label col row) = toJSON (col, row)

instance FromJSON Label where
  parseJSON v = do
    (col,row) <- parseJSON v
    return $ Label col row

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

instance ToJSON Player where
  toJSON (User name) = toJSON name
  toJSON (AI ai) = toJSON (aiName ai)

instance ToJSON SomeRules where
  toJSON (SomeRules rules) = toJSON (rulesName rules)

instance ToJSON GameStatus where
  toJSON New = toJSON ("New" :: T.Text)
  toJSON Running = toJSON ("Running" :: T.Text)
  toJSON (Ended result) = toJSON result

instance ToJSON Game where
  toJSON g =
    object ["id" .= getGameId g,
            "rules" .= gRules g,
            "status" .= gStatus g,
            "first" .= gPlayer1 g,
            "second" .= gPlayer2 g
          ]

-- instance FromJSON Player

instance FromJSON NewGameRq where
  parseJSON = withObject "NewGame" $ \v -> NewGameRq
    <$> v .: "rules"
    <*> v .:? "params" .!= Null
    <*> v .:? "board"
    <*> v .:? "fen"
    <*> v .:? "pdn"

instance FromJSON AttachAiRq where
  parseJSON = withObject "AttachAi" $ \v -> AttachAiRq
    <$> v .: "ai"
    <*> v .:? "params" .!= Null

instance ToJSON Notify where
  toJSON (MoveNotify to from move board) =
    object ["to_side" .= to, "from_side" .= from, "move" .= move, "board" .= board]
  toJSON (UndoNotify to from board) =
    object ["to_side" .= to, "from_side" .= from, "undo" .= True, "board" .= board]
  toJSON (ResultNotify to from result) =
    object ["to_side" .= to, "from_side" .= from, "result" .= result]

instance ToJSON RsPayload where
  toJSON (NewGameRs id) = object ["id" .= id]
  toJSON RegisterUserRs = object ["register_user" .= ("ok" :: T.Text)]
  toJSON AttachAiRs = object ["attach_ai" .= ("ok" :: T.Text)]
  toJSON RunGameRs = object ["run_game" .= ("ok" :: T.Text)]
  toJSON (PollRs messages) = toJSON messages
  toJSON (StateRs board side) = object ["board" .= board, "side" .= side]
  toJSON (PossibleMovesRs moves) = toJSON moves
  toJSON (MoveRs board) = toJSON board
  toJSON (UndoRs board) = toJSON board
  toJSON (LobbyRs games) = toJSON games
  toJSON (NotationRs size list) = object ["size" .= size, "notation" .= list]

instance ToJSON SupervisorRs where
  toJSON (SupervisorRs payload messages) = object ["response" .= payload, "messages" .= messages]

