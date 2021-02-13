{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ExercismWizard.Main
  ( main
  )
where

import qualified Data.Text as T
import ExercismWizard.CommandParse
import ExercismWizard.FSPath
import System.Exit
import Turtle.Prelude
import Prelude hiding (FilePath)

data ExercismCli = ExercismCli
  { binPath :: FilePath
  , workspace :: FilePath
  }
  deriving (Show)

{-
  Find infomation on existing exercism cli setup.
  This is also to confirm that the binary is installed and configured.
 -}
findCli :: IO ExercismCli
findCli = do
  Just binPath <- which "exercism"
  (ExitSuccess, out) <- procStrict (toText binPath) ["workspace"] ""
  let [fromText -> workspace] = T.lines out
  True <- testdir workspace
  pure ExercismCli {binPath, workspace}

main :: IO ()
main = do
  findCli >>= print
  getArgs >>= print
