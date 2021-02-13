module ExercismWizard.Main
  ( main
  )
where

import System.Process
import System.Posix.Daemon
{-
  TODO: find exercism binary and run its workspace command.
 -}

runThunar :: IO ()
runThunar = callProcess "thunar" []

main :: IO ()
main = runDetached Nothing DevNull runThunar
