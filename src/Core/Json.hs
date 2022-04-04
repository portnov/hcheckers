{-# LANGUAGE OverloadedStrings #-}
module Core.Json where

import Control.Concurrent
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Default
import qualified Data.HashMap.Strict as HM
import System.Log.Heavy

import Core.Types
import Core.Logging
import Core.Supervisor
import Formats.Types

instance ToJSON PlayerDirection where

instance ToJSON PieceKind

instance FromJSON PieceKind

instance ToJSON Side

instance FromJSON Side

instance ToJSON GameResult

instance FromJSON GameResult

instance ToJSON BoardOrientation

instance ToJSON BoardTopology

instance ToJSON AiSessionStatus where
  toJSON AiRunning = object ["status" .= ("running" :: T.Text)]
  toJSON AiStopping = object ["status" .= ("stopping" :: T.Text)]
  toJSON NoAiHere = object ["status" .= ("human" :: T.Text)]
  toJSON (AiDone rs) = object ["status" .= ("done" :: T.Text), "response" .= rs]

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

instance ToJSON HistoryRecordRep where
  toJSON r = object ["side" .= hrrSide r, "move" .= hrrMove r]

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
  toJSON (DrawRequested side) = object ["draw_requested" .= side]
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
    <*> v .:? "previous_board"

instance FromJSON AttachAiRq where
  parseJSON = withObject "AttachAi" $ \v -> AttachAiRq
    <$> v .: "ai"
    <*> v .:? "params" .!= Null

instance FromJSON PdnInfoRq where
  parseJSON = withObject "PDN" $ \v -> PdnInfoRq
    <$> v .: "rules"
    <*> v .: "text"

instance ToJSON PdnInfo where
  toJSON pdn = object [
        "title" .= pdnTitle pdn
      , "rules" .= pdnRules pdn
      , "result" .= pdnResult pdn
      , "next_move" .= pdnNextMove pdn
    ]

instance ToJSON Notify where
  toJSON (MoveNotify to from move board) =
    object ["to_side" .= to, "from_side" .= from, "move" .= move, "board" .= board]
  toJSON (UndoNotify to from board) =
    object ["to_side" .= to, "from_side" .= from, "undo" .= True, "board" .= board]
  toJSON (AiHintNotify to from moves) =
    object ["to_side" .= to, "from_side" .= from, "hint" .= moves]
  toJSON (ResultNotify to from result) =
    object ["to_side" .= to, "from_side" .= from, "result" .= result]
  toJSON (DrawRqNotify to from) =
    object ["to_side" .= to, "from_side" .= from, "draw" .= ("requested" :: T.Text)]
  toJSON (DrawRsNotify to from accepted) =
    object ["to_side" .= to, "from_side" .= from, "draw_accepted" .= accepted]
  toJSON (LogNotify to level src text) =
    object ["to_side" .= to, "source" .= src, "level" .= level, "message" .= text]

instance ToJSON RsPayload where
  toJSON (NewGameRs id side) = object ["id" .= id, "turn" .= side]
  toJSON RegisterUserRs = object ["register_user" .= ("ok" :: T.Text)]
  toJSON AttachAiRs = object ["attach_ai" .= ("ok" :: T.Text)]
  toJSON AttachSpectatorRs = object ["attach_spectator" .= ("ok" :: T.Text)]
  toJSON RunGameRs = object ["run_game" .= ("ok" :: T.Text)]
  toJSON RunGameLoopRs = object ["run_game_loop" .= ("ok" :: T.Text)]
  toJSON (PollRs messages) = toJSON messages
  toJSON (StateRs board status side) = object ["board" .= board, "side" .= side, "status" .= status]
  toJSON (HistoryRs records) = toJSON records
  toJSON (HistoryBoardRs board) = toJSON board
  toJSON (PdnInfoRs info) = toJSON info
  toJSON (PossibleMovesRs moves) = toJSON moves
  toJSON (MoveRs board sessionId) = object ["board" .= board, "poll" .= sessionId]
  toJSON (PollMoveRs status) = toJSON status
  toJSON (AiHintRs sessionId) = object ["poll" .= sessionId]
  toJSON (StopAiRs) = object ["stop" .= ("ok" :: T.Text)]
  toJSON (UndoRs board) = toJSON board
  toJSON CapitulateRs = object ["capitulate" .= ("ok" :: T.Text)]
  toJSON (DrawRqRs sessionId) = object ["draw_request" .= ("pending" :: T.Text), "poll" .= sessionId]
  toJSON (DrawAcceptRs accepted) = object ["draw_accepted" .= accepted]
  toJSON (LobbyRs games) = toJSON games
  toJSON (NotationRs size orientation list border) =
      object ["size" .= size, "orientation" .= orientation, "notation" .= list, "border_notation" .= border]
  toJSON (TopologyRs topology) =
      object ["topology" .= topology]
  toJSON ShutdownRs = object ["shutdown" .= ("ok" :: T.Text)]

instance ToJSON SideNotation where
  toJSON sn = object [
                "top" .= snTopLabels sn
              , "left" .= snLeftLabels sn
              , "bottom" .= snBottomLabels sn
              , "right" .= snRightLabels sn
            ]

instance ToJSON Response where
  toJSON (Response payload messages) = object ["response" .= payload, "messages" .= messages]

instance FromJSON AiConfig where
  parseJSON = withObject "AiConfig" $ \v -> AiConfig
      <$> v .:? "threads" .!= (aiThreads def)
      <*> v .:? "load" .!= (aiLoadCache def)
      <*> v .:? "store" .!= (aiStoreCache def)
      <*> v .:? "use_cache_max_depth" .!= (aiUseCacheMaxDepth def)
      <*> v .:? "use_cache_max_pieces" .!= (aiUseCacheMaxPieces def)
      <*> v .:? "use_cache_max_depth_plus" .!= (aiUseCacheMaxDepthPlus def)
      <*> v .:? "use_cache_max_depth_minus" .!= (aiUseCacheMaxDepthMinus def)
      <*> v .:? "update_cache_max_depth" .!= (aiUpdateCacheMaxDepth def)
      <*> v .:? "update_cache_max_pieces" .!= (aiUpdateCacheMaxPieces def)

instance FromJSON BattleServerConfig where
  parseJSON = withObject "BattleServerConfig" $ \v -> BattleServerConfig
    <$> v .:? "enable" .!= bsEnable def
    <*> v .:? "host" .!= bsHost def
    <*> v .:? "port" .!= bsPort def

instance FromJSON GeneralConfig where
  parseJSON = withObject "GeneralConfig" $ \v -> GeneralConfig
    <$> v .:? "host" .!= (gcHost def)
    <*> v .:? "port" .!= (gcPort def)
    <*> v .:? "local" .!= (gcLocal def)
    <*> v .:? "enable_metrics" .!= (gcEnableMetrics def)
    <*> v .:? "metrics_port" .!= (gcMetricsPort def)
    <*> v .:? "log_path" .!= (gcLogFile def)
    <*> v .:? "log_level" .!= (gcLogLevel def)
    <*> v .:? "ai" .!= (gcAiConfig def)
    <*> v .:? "battle_server" .!= (gcBattleServerConfig def)

instance FromJSON Level where
  parseJSON (String "debug") = return debug_level
  parseJSON (String "verbose") = return verbose_level
  parseJSON (String "trace") = return trace_level
  parseJSON (String "info") = return info_level
  parseJSON (String "warning") = return warn_level
  parseJSON (String "error") = return error_level
  parseJSON (String "fatal") = return fatal_level
  parseJSON (String "disable") = return disable_logging
  parseJSON invalid = typeMismatch "logging level" invalid

mergeObjects :: Value -> Value -> Value
mergeObjects (Object v1) (Object v2) = Object (HM.union v1 v2)
mergeObjects _ _ = error "not object value"

