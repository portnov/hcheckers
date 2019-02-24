{-# LANGUAGE OverloadedStrings #-}
module Core.Config
  (locateConfig, loadConfig
  ) where

import Data.Yaml
import Data.Default
import System.FilePath
import System.Directory
import System.Environment

import Core.Types
import Core.CmdLine
import Core.Json () -- import instances only

locateConfig :: IO (Maybe FilePath)
locateConfig = do
  home <- getEnv "HOME"
  let homePath = home </> ".config" </> "hcheckers" </> "server.yaml"
  ex <- doesFileExist homePath
  if ex
    then return $ Just homePath
    else do
      let etcPath = "/etc" </> "hcheckers" </> "server.yaml"
      ex <- doesFileExist etcPath
      if ex
        then return $ Just etcPath
        else return Nothing

loadConfig :: CmdLine -> IO GeneralConfig
loadConfig cmd = do
  mbPath <-
      case cmdConfigPath cmd of
          Nothing -> locateConfig
          Just path -> return (Just path)
  config <-
      case mbPath of
        Nothing -> return def
        Just path -> do
          putStrLn $ "Using config: " ++ path
          r <- decodeFileEither path
          case r of
            Left err -> fail $ show err
            Right cfg -> return cfg
  let config' = case cmdLocal cmd of
                  Nothing -> config
                  Just local -> config {gcLocal = local}
  return config'

