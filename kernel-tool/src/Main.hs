{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import System.Process
import System.Environment
import Turtle

main :: IO ()
main = do
    cd "/"
    shBin <- getEnv "SHELL"
    setEnv "KERNEL_TOOL" "Test"
    let p = (System.Process.proc shBin [])
            { std_in  = Inherit
            , std_out = Inherit
            , std_err = Inherit
            , delegate_ctlc = True
            }
    (_, _, _, ph) <- createProcess p
    _ <- waitForProcess ph
    pure ()
