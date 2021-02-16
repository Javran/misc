{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( main
  ) where

import Prelude hiding (FilePath)
import Turtle.Prelude
import Turtle.Shell
import Filesystem.Path.CurrentOS
import qualified Data.Text as T

fpToText :: FilePath -> T.Text
fpToText = either id id . Filesystem.Path.CurrentOS.toText


{-
  Options are passed from environment variables:

  - EXERCISM_OLD_REPO_HASKELL
  - EXERCISM_NEW_REPO_HASKELL

  Migration plan:

  - Fetch all

    + Scan old Haskell repo and call exercism to fetch the corresponding new one.
    + update stack.yaml if needed to use `resolver: lts-16.3`.

  - Move files

    + Looks like most of the project home consists of 3 files:
      a README, the source code and a test file. We'll compare the source code file
      with one found in newly fetched project home.
      This is just a sanity check that those two files are indeed identical.
    + Now in new exercise project home, move old source file under src/.
      Usually there's a single file under that directory here we can also check its name.
    + Place a migration marker "MIGRATION_MARKER" with content: "TODO: migrated"
      under project home.
      This is to make sure that when we are doing with the migration
      everything are built and tested properly.
    + the presence of migration marker is also a indication that old directory
      can be removed safely, this is the last step of migrating an exercise.

 -}

main :: IO ()
main = do
  Just oldRepo <- fmap fromText <$> need "EXERCISM_OLD_REPO_HASKELL"
  sh $ do
    z <- ls oldRepo
    liftIO $ print $ fpToText (filename z)
