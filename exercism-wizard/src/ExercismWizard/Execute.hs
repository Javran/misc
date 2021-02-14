{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module ExercismWizard.Execute
  ( findCli
  , ExercismCli (..)
  , execute
  )
where

import Control.Monad
import qualified Data.Text as T
import ExercismWizard.CommandParse
import ExercismWizard.FSPath
import System.Exit
import System.FilePath.Posix (pathSeparator)
import Turtle.Prelude
import Prelude hiding (FilePath)

data ExercismCli = ExercismCli
  { -- | Binary path to exercism CLI
    binPath :: FilePath
  , -- | Path to exercism workspace
    workspace :: FilePath
  , -- | Canonicalized path to exercism workspace
    workspaceReal :: FilePath
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
  workspaceReal <- realpath workspace
  pure ExercismCli {binPath, workspace, workspaceReal}

data Exercise = Exercise
  { langTrack :: T.Text -- TODO: use LangTrack
  , name :: T.Text
  , projectHome :: FilePath
  }
  deriving (Show)

guessExercise :: ExercismCli -> IO (Maybe Exercise)
guessExercise ExercismCli {workspaceReal, workspace} = do
  cwd <- pwd >>= realpath
  let lePair = do
        {-
           stripPrefix has a rather unintuitive behavior:

           > stripPrefix "/a/b" "/a/b/c"
           Nothing
           > stripPrefix "/a/b/" "/a/b/c"
           Just (FilePath "c")

           This is due to stripPrefix splits a path into (directory,basename and extension) pair,
           while canonicalized path does not have the trailing path separator.
           The fix is to append an "" after the prefix path.
        -}
        xs <- stripPrefix (workspaceReal </> "") cwd
        lPre : ePre : _ <- pure $ fmap toText $ splitDirectories xs
        (l, lSep) <- T.unsnoc lPre
        guard $ lSep == pathSeparator
        (e, eSep) <- T.unsnoc ePre
        guard $ eSep == pathSeparator
        pure (l, e)
  case lePair of
    Just (langTrack, name) -> do
      let checkMeta = True
      let projectHome = workspace </> fromText langTrack </> fromText name
      e <-
        if checkMeta
          then testdir $ projectHome </> ".exercism"
          else pure True
      pure $ guard e >> Just Exercise {langTrack, name, projectHome}
    Nothing -> pure Nothing

execute :: ExercismCli -> Command -> IO ()
execute e@ExercismCli {binPath} cmd = case cmd of
  CmdProxy args -> proc (toText binPath) args "" >>= exitWith
  _ -> do
    print e
    putStrLn "Not yet supported:"
    print cmd
    guessExercise e >>= print
    exitFailure
