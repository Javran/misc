{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ExercismWizard.Main
  ( main
  )
where

import qualified Data.Text as T
import ExercismWizard.FSPath
import System.Exit
import Turtle.Prelude
import Prelude hiding (FilePath)

data Cli = Cli
  { binPath :: FilePath
  , workspace :: FilePath
  }
  deriving (Show)

{-
  Find infomation on existing exercism cli setup.
  This is also to confirm that the binary is installed and configured.
 -}
findCli :: IO Cli
findCli = do
  Just binPath <- which "exercism"
  (ExitSuccess, out) <- procStrict (toText binPath) ["workspace"] ""
  let [fromText -> workspace] = T.lines out
  True <- testdir workspace
  pure Cli {binPath, workspace}

main :: IO ()
main = findCli >>= print
