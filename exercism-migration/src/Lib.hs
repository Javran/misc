{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified Control.Foldl as Fold
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Filesystem.Path.CurrentOS
import System.Exit
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell
import Prelude hiding (FilePath)

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

lsExercise :: FilePath -> Shell (T.Text, FilePath)
lsExercise repo = do
  exerRepo <- ls repo
  let eName = fpToText (filename exerRepo)
  pure (eName, exerRepo)

_mainFetchAll :: IO ()
_mainFetchAll = do
  Just oldRepo <- fmap fromText <$> need "EXERCISM_OLD_REPO_HASKELL"
  sh $ do
    exerRepo <- ls oldRepo
    let eName = fpToText (filename exerRepo)
    liftIO $ T.putStrLn $ "Fetching " <> eName
    (ec, out) <-
      procStrict "exercism" ["download", "--exercise=" <> eName, "--track=haskell"] ""
    unless (ec == ExitSuccess) $
      liftIO $ do
        print ec
        T.putStrLn out

mainVerifyOrMoveAll :: IO ()
mainVerifyOrMoveAll = do
  Just oldRepo <- fmap fromText <$> need "EXERCISM_OLD_REPO_HASKELL"
  Just newRepo <- fmap fromText <$> need "EXERCISM_NEW_REPO_HASKELL"
  sh $ do
    (eName, eOldPath) <- lsExercise oldRepo
    let eNewPath = newRepo </> fromText eName
    e <- testdir eNewPath
    if e
      then do
        liftIO $ T.putStrLn $ "Verifying " <> eName <> "..."
        srcFiles <- reduce Fold.list $ do
          oldSrcFile <- find (suffix ".hs") eOldPath
          [] <- pure $ match (suffix "_test.hs") (fpToText oldSrcFile)
          pure oldSrcFile
        case srcFiles of
          [oldSrcFile] -> do
            let fName = filename oldSrcFile
                newSrcFile = newRepo </> fromText eName </> fName
            b <- testfile newSrcFile
            if b
              then do
                (ec, out) <-
                  procStrict
                    "diff"
                    ["-b", "-B", fpToText oldSrcFile, fpToText newSrcFile]
                    ""
                if ec == ExitSuccess
                  then do
                    -- at this point: single source file in old dir, no diff.
                    newTargetSrcFiles <- reduce Fold.list $ ls (eNewPath </> "src")
                    case newTargetSrcFiles of
                      [newTargetSrcFile]
                        | filename newTargetSrcFile == filename oldSrcFile -> do
                          -- time to perform the move
                          mv oldSrcFile newTargetSrcFile
                          rm newSrcFile
                          liftIO $
                            T.writeFile
                              (encodeString $ newRepo </> fromText eName </> "MIGRATION_MARKER")
                              "TODO: migrated"
                          pure ()
                      _ -> echo "  Unexpected: should only have one file or file name differs."
                  else liftIO $ T.putStrLn out
              else liftIO $ putStrLn "  New source file not found."
          _ -> liftIO $ putStrLn $ "Found unexpected number of files: " <> show srcFiles
      else liftIO $ T.putStrLn $ "Skipping " <> eName <> ", new path not found."

main :: IO ()
main = mainVerifyOrMoveAll
