
module Core.CmdLine where

import Options.Applicative
import Data.Char (toLower)

data CmdCommand =
    RunGameServer { cmdLocal :: Maybe Bool }
  | RunBattleServer
  | SpecialCommand String
  deriving (Eq, Show)

data CmdLine = CmdLine {
      cmdConfigPath :: Maybe FilePath
    , cmdMetrics :: Maybe Bool
    , cmdDumpConfig :: Bool
    , cmdCommand :: CmdCommand
    }
  | CmdVersion
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
cmdline =
    (CmdLine
      <$> optional (strOption
            ( long "config"
             <> short 'c'
             <> metavar "PATH"
             <> help "Path to the config file" ) )
      <*> optional (option bool
            ( long "metrics"
              <> metavar "on|off"
              <> help "Enable or disable metrics monitoring. This command-line option overrides one specified in the config file.")
            )
      <*> switch
              ( long "dump-config"
                <> help "Print resulting configuration parameters (including both ones read from file and parsed from command line)"
              )
      <*> parseCommand
    ) <|> flag' CmdVersion
              ( long "version"
                <> short 'v'
                <> help "Display version information and exit"
              )

parseCommand :: Parser CmdCommand
parseCommand =
    (RunGameServer
      <$> optional (option bool
            ( long "local"
              <> short 'L'
              <> metavar "on|off"
              <> help "Run server in local mode" )
            )
    ) <|> (flag' RunBattleServer
              ( long "battle"
                <> short 'B'
                <> help "Run `battle server' instead of normal game server")
    ) <|> (SpecialCommand
          <$> strOption
                ( long "special"
                 <> metavar "COMMAND"
                 <> help "Run special command (undocumented)"
                )
    )

parserInfo :: ParserInfo CmdLine
parserInfo = info (cmdline <**> helper)
  ( fullDesc
    <> progDesc "HCheckers server application"
    <> header "Run HCheckers server"
    <> footer "Use `+RTS options' after all HCheckers parameters to specify options for GHC Runtime, such as amount of heap to be used; for example, `hcheckersd --local=on +RTS -H1G'. Use `hcheckersd +RTS -?' to display help about Runtime options.")

