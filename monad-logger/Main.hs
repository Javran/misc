#!/usr/bin/env stack
-- stack --resolver lts-13.2 --install-ghc runghc --package monad-logger
{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Main where

import Control.Monad.Logger
import qualified Data.Text as T

lD :: MonadLogger m => T.Text -> m ()
lD = $(logDebug)

lE :: MonadLogger m => T.Text -> m ()
lE = $(logError)

logTest :: MonadLogger m => m ()
logTest = do
    lD "D foo"
    lE "E err"
    logDebugN "D"
    logInfoN "I"
    logWarnN "W"
    logErrorN "E"

main :: IO ()
main = runStderrLoggingT logTest
