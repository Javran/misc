module Common
  ( shDive
  ) where

import System.Exit
import System.Process

shDive :: FilePath -> [String] -> IO ExitCode
shDive binPath args = do
  (_, _, _, ph) <- createProcess $
    (System.Process.proc binPath args)
      { std_in  = Inherit
      , std_out = Inherit
      , std_err = Inherit
      , delegate_ctlc = True
      }
  waitForProcess ph
