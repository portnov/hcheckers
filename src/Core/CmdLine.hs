
module Core.CmdLine where

import Options.Applicative
import Data.Semigroup ((<>))

data CmdLine = CmdLine {
      cmdConfigPath :: Maybe FilePath
    , cmdLocal :: Maybe Bool
    , cmdSpecial :: Maybe String
    }
  deriving (Eq, Show)

cmdline :: Parser CmdLine
cmdline = CmdLine
  <$> optional (strOption
        ( long "config"
         <> short 'c'
         <> metavar "PATH"
         <> help "Path to the config file" ) )
  <*> optional (switch
        ( long "local"
         <> short 'L'
         <> help "Run server in local mode" ) )
  <*> optional (strOption
        ( long "special"
         <> metavar "COMMAND" ) )

parserInfo :: ParserInfo CmdLine
parserInfo = info (cmdline <**> helper)
  ( fullDesc
    <> progDesc "HCheckers server application"
    <> header "Run HCheckers server" )

