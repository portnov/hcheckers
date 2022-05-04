{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Logging where

import Language.Haskell.TH.Syntax
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy
import System.Log.Heavy.TH

verbose_level :: Level
verbose_level = Level "VERBOSE" 700 Syslog.Debug

config_level :: Level
config_level = Level "CONFIG" 650 Syslog.Debug

verbose :: Q Exp
#ifdef VERBOSE
verbose = putMessage trace_level
#else
verbose = [| \fmt args -> return () |]
#endif

traceConfig :: Q Exp
traceConfig = putMessage config_level

infoOrDebug :: Q Exp
infoOrDebug = [|
      \isInfo fmt args -> if isInfo
                            then $info fmt args
                            else $debug fmt args
  |]

