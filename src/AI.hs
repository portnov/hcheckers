{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module AI where

import Data.Maybe
import Data.Default
import Data.Aeson

import Core.Types
import Core.Board
import Core.Logging
import Core.Supervisor

loadAi :: String -> SomeRules -> FilePath -> IO SomeAi
loadAi name rules path = do
  r <- decodeFileStrict path
  case r of
    Nothing -> fail $ "Cannot load AI from " ++ path
    Just value ->
      case selectAi (AttachAiRq name value) rules of
        Nothing -> fail $ "Unknown AI: " ++ name
        Just ai -> return ai

