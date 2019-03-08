
module Core.CmdLine where

import Options.Applicative
import Data.Semigroup ((<>))
import Data.Char (toLower)

data CmdLine = CmdLine {
      cmdConfigPath :: Maybe FilePath
    , cmdLocal :: Maybe Bool
    , cmdSpecial :: Maybe String
    }
  deriving (Eq, Show)

bool :: ReadM Bool
bool = eitherReader $ \str ->
  case map toLower str of
    "on" -> Right True
    "true" -> Right True
    "off" -> Right False
    "false" -> Right False
    _ -> Left $ "Unknown boolean value: " ++ str

cmdline :: Parser CmdLine
cmdline = CmdLine
  <$> optional (strOption
        ( long "config"
         <> short 'c'
         <> metavar "PATH"
         <> help "Path to the config file" ) )
  <*> optional (option bool
        ( long "local"
         <> short 'L'
         <> metavar "on|off"
         <> help "Run server in local mode" ) )
  <*> optional (strOption
        ( long "special"
         <> metavar "COMMAND" ) )

parserInfo :: ParserInfo CmdLine
parserInfo = info (cmdline <**> helper)
  ( fullDesc
    <> progDesc "HCheckers server application"
    <> header "Run HCheckers server" )

