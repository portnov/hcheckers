{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}

module AI where

import Data.Default
import Data.Aeson

import Core.Types
import Core.Supervisor () -- import instances only
import AI.AlphaBeta.Types

loadAi :: (GameRules rules) => String -> rules -> FilePath -> IO (AlphaBeta rules (EvaluatorForRules rules))
loadAi name rules path = do
  r <- decodeFileStrict path
  case r of
    Nothing -> fail $ "Cannot load AI from " ++ path
    Just value -> do
      let rules' = SomeRules rules
          ai = AlphaBeta def rules (dfltEvaluator rules)
      return $ updateAi ai value

