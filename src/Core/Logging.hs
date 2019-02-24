{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Logging where

import Language.Haskell.TH.Syntax
import qualified System.Posix.Syslog as Syslog
import System.Log.Heavy

verbose_level :: Level
verbose_level = Level "VERBOSE" 700 Syslog.Debug

verbose :: Q Exp
#ifdef VERBOSE
verbose = putMessage trace_level
#else
verbose = [| \fmt args -> return () |]
#endif

