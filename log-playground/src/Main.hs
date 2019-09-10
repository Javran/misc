{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

{-
  Experiment some log-to-file facility
  together with UNIX file permission setup and stuff.
 -}

import Prelude hiding (log)
import Control.Logging
import Control.Monad
import System.Posix.Files

main :: IO ()
main = do
    {-
      To make sure that the file has the right permission bit,
      a quick and simple solution might be just to set those bits prior
      to any logging events.
      But admittedly this isn't really a good solution, perhaps don't bother
      with making this right at this moment.
     -}
    when fileLogging $
      setFileMode logFilePath 0o600
    loggingContext $ do
      {-
        see: Data.Time.Format
       -}
      setLogTimeFormat "%F %T"
      log "stuff"
      logS "uvuv" "fff" -- xxxS family provides an additional source to print out.
      warnS "component" "warning message."
      pure ()
  where
    logFilePath = "/tmp/log-playground.log"
    fileLogging = True
    loggingContext =
      if fileLogging
        then withFileLogging logFilePath
        else withStdoutLogging
