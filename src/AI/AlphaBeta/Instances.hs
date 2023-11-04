{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}

module AI.AlphaBeta.Instances where

import qualified Data.Text as T
import Data.Default
import Data.Aeson
import Data.Aeson.Types (unexpected)
import qualified Data.Aeson.KeyMap as KM

import AI.AlphaBeta.Types

instance ToJSON DrawPolicy where
  toJSON AlwaysAccept = toJSON ("always" :: T.Text)
  toJSON AlwaysDecline = toJSON ("never" :: T.Text)
  toJSON AcceptIfLosing = toJSON ("if_losing" :: T.Text)

instance FromJSON DrawPolicy where
  parseJSON = withText "accept_draw" $ \v ->
    case v of
      "always" -> return AlwaysAccept
      "never" -> return AlwaysDecline
      "if_losing" -> return AcceptIfLosing
      _ -> unexpected (String v)

instance FromJSON AlphaBetaParams where
  parseJSON = withObject "AlphaBetaParams" $ \v -> AlphaBetaParams
      <$> v .: "depth"
      <*> v .:? "start_depth"
      <*> v .:? "max_combination_depth" .!= 8
      <*> v .:? "dynamic_depth" .!= abDynamicDepth def
      <*> v .:? "depth_step" .!= abDepthStep def
      <*> v .:? "deeper_if_bad" .!= False
      <*> v .:? "depth_if_ambigous"
      <*> v .:? "moves_bound_low" .!= 4
      <*> v .:? "moves_bound_high" .!= 8
      <*> v .:? "init_window_width" .!= abInitWindowWidth def
      <*> v .:? "time"
      <*> v .:? "random_opening_depth" .!= abRandomOpeningDepth def
      <*> v .:? "random_opening_options" .!= abRandomOpeningOptions def
      <*> v .:? "accept_draw" .!= abDrawPolicy def
      <*> v .:? "background_thinking" .!= abThinkInBackground def

instance ToJSON AlphaBetaParams where
  toJSON p = object [
              "depth" .= abDepth p,
              "start_depth" .= abStartDepth p,
              "max_combination_depth" .= abCombinationDepth p,
              "dynamic_depth" .= abDynamicDepth p,
              "depth_step" .= abDepthStep p,
              "deeper_if_bad" .= abDeeperIfBad p,
              "depth_if_ambigous" .= abDeeperIfAmbigous p,
              "moves_bound_low" .= abMovesLowBound p,
              "moves_bound_high" .= abMovesHighBound p,
              "init_window_depth" .= abInitWindowWidth p,
              "time" .= abBaseTime p,
              "random_opening_depth" .= abRandomOpeningDepth p,
              "random_opening_options" .= abRandomOpeningOptions p,
              "accept_draw" .= abDrawPolicy p,
              "background_thinking" .= abThinkInBackground p
            ]

instance ToJSON eval => ToJSON (AlphaBeta rules eval) where
  toJSON (AlphaBeta params rules eval) =
    let Object paramsV = toJSON params
        Object evalV = toJSON eval
    in  Object $ KM.union paramsV evalV

