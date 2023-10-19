{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Version (
    Version (..),
    showVersion,
    getVersion,
    printVersion
  ) where

import qualified Data.Text as T
import qualified Data.Version as V
import Text.Printf
import qualified GitHash as G
import qualified Paths_hcheckers as Paths (version)

data Version = Version {
    vRelease :: T.Text,
    vHash :: T.Text
  }
  deriving (Eq, Show)

showVersion :: Version -> T.Text
showVersion v = T.pack $ printf "%s (%s)" (vRelease v) (vHash v)

getVersion :: Version
getVersion =
  let packageVersion = T.pack $ V.showVersion Paths.version
      info = $$G.tGitInfoCwdTry
      gitHash = case info of
                  Left _ -> "not git"
                  Right gitInfo -> T.pack $ take 8 $ G.giHash gitInfo
  in  Version packageVersion gitHash

printVersion :: IO ()
printVersion = putStrLn $ T.unpack $ showVersion getVersion

