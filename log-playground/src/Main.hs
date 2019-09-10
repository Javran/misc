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

main :: IO ()
main = withFileLogging "/tmp/log-playground.log" $ do
  log "stuff"
  log "aaa"
  log "bbb"
  logS "uvuv" "fff"
  warnS "component" "warning message."
  pure ()
