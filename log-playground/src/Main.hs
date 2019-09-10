{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

{-
  Experiment some log-to-file facility
  together with UNIX file permission setup and stuff.

  TODO: mechanism to make sure that the file has rw- --- --- ?
 -}

import Prelude hiding (log)
import Control.Logging

main :: IO ()
main = loggingContext $ do
  -- see: Data.Time.Format
  setLogTimeFormat "%F %T"
  log "stuff"
  log "aaa"
  log "bbb"
  logS "uvuv" "fff" -- xxxS family provides an additional source to print out.
  warnS "component" "warning message."
  pure ()
  where
    fileLogging = False
    loggingContext =
      if fileLogging
        then withFileLogging  "/tmp/log-playground.log"
        else withStdoutLogging
