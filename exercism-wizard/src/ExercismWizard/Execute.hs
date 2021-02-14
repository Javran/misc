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
import Data.Maybe
import qualified Data.Text as T
import ExercismWizard.CommandParse
import ExercismWizard.FSPath
import ExercismWizard.Language (LangTrack, parseLangTrack)
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
  { langTrack :: LangTrack
  , name :: T.Text
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
        xs <- stripPrefix (workspaceReal </> "") (cwd </> "")
        lPre : ePre : _ <- pure $ fmap toText $ splitDirectories xs
        (l, lSep) <- T.unsnoc lPre
        guard $ lSep == pathSeparator
        (e, eSep) <- T.unsnoc ePre
        guard $ eSep == pathSeparator
        pure (l, e)
  case lePair of
    Just (langTrackRaw, name) -> do
      let checkMeta = True
      e <-
        if checkMeta
          then
            let projectHome = workspace </> fromText langTrackRaw </> fromText name
             in testdir $ projectHome </> ".exercism"
          else pure True
      pure $ do
        langTrack <- parseLangTrack langTrackRaw
        guard e >> Just Exercise {langTrack, name}
    Nothing -> pure Nothing

fillExercise :: ExercismCli -> RawExercise -> IO (Maybe Exercise)
fillExercise ec (RawExercise (l, e)) = case (l, e) of
  (Just l', Just e') -> pure (Just $ Exercise l' e')
  _ -> do
    guessed <- guessExercise ec
    pure $ do
      Exercise {langTrack = gl, name = gn} <- guessed
      pure $ Exercise (fromMaybe gl l) (fromMaybe gn e)

execute :: ExercismCli -> Command -> IO ()
execute e@ExercismCli {binPath} cmd = case cmd of
  CmdProxy args -> proc (toText binPath) args "" >>= exitWith
  CmdTest rawExer -> fillExercise e rawExer >>= print
  _ -> do
    print e
    putStrLn "Not yet supported:"
    print cmd
    exitFailure
