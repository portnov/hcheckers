
module Core.CmdLine where

import Options.Applicative
import Data.Char (toLower)

data CmdLine = CmdLine {
      cmdConfigPath :: Maybe FilePath
    , cmdLocal :: Maybe Bool
    , cmdBattleServer :: Maybe Bool
    , cmdMetrics :: Maybe Bool
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
  <*> optional (option bool
        ( long "battle"
          <> short 'B'
          <> metavar "on|off"
          <> help "Run `battle server' instead of normal game server") )
  <*> optional (option bool
        ( long "metrics"
          <> metavar "on|off"
          <> help "Enable or disable metrics monitoring. This command-line option overrides one specified in the config file.")
        )
  <*> optional (strOption
        ( long "special"
         <> metavar "COMMAND" ) )

parserInfo :: ParserInfo CmdLine
parserInfo = info (cmdline <**> helper)
  ( fullDesc
    <> progDesc "HCheckers server application"
    <> header "Run HCheckers server" )

