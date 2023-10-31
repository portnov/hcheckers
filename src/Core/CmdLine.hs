
module Core.CmdLine where

import Options.Applicative
import Data.Char (toLower)

data ServerCommand =
    RunGameServer { cmdLocal :: Maybe Bool }
  | RunBattleServer
  deriving (Eq, Show)

data SpecialCommand =
      Learn {
          scRulesName :: String
        , scAiPath :: FilePath
        , scPdnPath :: FilePath
      }
    | Openings {
          scRulesName :: String
        , scAiPath :: FilePath
        , scDepth :: Int
      }
    | Bench {
          scRulesName :: String
        , scAiPath :: FilePath
        , scCount :: Int
      }
    | Battle {
          scRulesName :: String
        , scHost :: Maybe String
        , scAiPath1 :: FilePath
        , scAiPath2 :: FilePath
      }
    | Match {
          scRulesName :: String
        , scCount :: Int
        , scAiPath1 :: FilePath
        , scAiPath2 :: FilePath
      }
    | Tournament {
          scMatchesCount :: Int
        , scGamesCount :: Int
        , scAiPaths :: [FilePath]
      }
    | Olympics {
          scRulesName :: String
        , scGamesCount :: Int
        , scNBest :: Int
        , scAiPaths :: [FilePath]
      }
    | Genetics {
          scYamlPath :: FilePath
      }
    | Generate {
          scCount :: Int
        , scDelta :: Int
        , scAiPath :: FilePath
      }
    | Dump {
          scDataPath :: FilePath
      }
  deriving (Eq, Show)

data CmdLine c = CmdLine {
      cmdConfigPath :: Maybe FilePath
    , cmdMetrics :: Maybe Bool
    , cmdDumpConfig :: Bool
    , cmdCommand :: c
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

cmdline :: Parser c -> Parser (CmdLine c)
cmdline command =
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
      <*> command
    ) <|> flag' CmdVersion
              ( long "version"
                <> short 'v'
                <> help "Display version information and exit"
              )

parseServerCommand :: Parser ServerCommand
parseServerCommand =
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
    )

parseSpecialCommand :: Parser SpecialCommand
parseSpecialCommand = hsubparser (
       cmd "learn" learn "learn"
    <> cmd "openings" openings "Generate openings data"
    <> cmd "bench" bench "benchmark"
    <> cmd "battle" battle "run battle"
    -- <> cmd "remote-battle" remoteBattle "run battle on a remote host"
    <> cmd "match" match "run match"
    <> cmd "tournament" tournament "run a tournament by a simple scheme"
    <> cmd "olympics" olympics "run a tournament based on olympics scheme"
    <> cmd "genetics" genetics "run genetics search"
    <> cmd "generate" generate "generate AI variants for genetic search"
    <> cmd "dump" dump "dump cache file"
  )
  where
    cmd name func helpText = command name (info func (progDesc helpText))

    parseRules = strArgument (metavar "RULES" <> help "Rules name")
    parseAiPath = strArgument (metavar "FILE.JSON" <> help "Path to AI settings")
    parseCount meta helpText = argument auto (metavar meta <> help helpText)

    learn :: Parser SpecialCommand
    learn = Learn
      <$> parseRules
      <*> parseAiPath
      <*> strArgument (metavar "FILE.PDN" <> help "Path to PDN file")

    openings :: Parser SpecialCommand
    openings = Openings
      <$> parseRules
      <*> parseAiPath
      <*> argument auto (metavar "DEPTH" <> help "Openings depth")

    bench :: Parser SpecialCommand
    bench = Bench
      <$> parseRules
      <*> parseAiPath
      <*> parseCount "COUNT" "count"

    battle :: Parser SpecialCommand
    battle = Battle
      <$> parseRules
      <*> optional (strOption
              (    long "remote"
                <> metavar "HTTP://HOST:8865"
                <> help "Battle server URL"
              )
            )
      <*> parseAiPath
      <*> parseAiPath

    match :: Parser SpecialCommand
    match = Match
      <$> parseRules
      <*> parseCount "N" "Number of battles"
      <*> parseAiPath
      <*> parseAiPath

    tournament :: Parser SpecialCommand
    tournament = Tournament
      <$> parseCount "N" "Number of matches in tournament"
      <*> parseCount "M" "Number of games in each match"
      <*> many parseAiPath

    olympics :: Parser SpecialCommand
    olympics = Olympics
      <$> parseRules
      <*> parseCount "N" "Number of games in each match"
      <*> parseCount "B" "Number of best AIs to select (expected to be power of 2)"
      <*> many parseAiPath

    genetics :: Parser SpecialCommand
    genetics = Genetics
      <$> strArgument (metavar "SETTINGS.YAML" <> help "Settings for genetics algorithm")

    generate :: Parser SpecialCommand
    generate = Generate
      <$> parseCount "N" "Number of AI configurations to generate"
      <*> parseCount "DELTA" "Variation value"
      <*> parseAiPath

    dump :: Parser SpecialCommand
    dump = Dump
      <$> strArgument (metavar "FILE.CACHE")

