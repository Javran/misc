{-# LANGUAGE OverloadedStrings #-}
module CommandClean
  ( cmdClean
  ) where

import Control.Monad
import Turtle.Prelude
import Turtle.Shell
import Data.Char
import Data.List
import Turtle.Pattern
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Control.Foldl as Foldl
import Control.Applicative

{-
  scan files directly under boot directory, match them against a given set
  of file name prefixes, and return a Map from kernel versions to
  a Map from matched file name prefixes to the file path of the file in question.

  example:

  > scanKernelFiles ["vmlinuz","config","System.map"] "/boot/"

  note that the given list of file name prefixes should not contain duplicates.
 -}
scanKernelFiles :: [T.Text] -> FP.FilePath -> Shell (M.Map T.Text (M.Map T.Text FP.FilePath))
scanKernelFiles prefixes bootDir =
    reduce (Foldl.Fold step initial id) $
      ls bootDir >>= \fp -> testfile fp >>= guard >> pure fp
  where
    patterns :: [Pattern (T.Text {- version -}, T.Text {- which prefix -})]
    patterns = toPattern <$> prefixes
      where
        toPattern :: T.Text -> Pattern (T.Text, T.Text)
        toPattern pref = do
          _ <- prefix (text pref)
          _ <- char '-'
          ver <- T.pack <$> some (satisfy (not . isSpace))
          eof
          pure (ver, pref)
    step curMap fp =
        case patTests of
          [] -> curMap
          (ver,whichPrefix):_ ->
            let doAlter Nothing = Just (M.singleton whichPrefix fp)
                doAlter (Just x) = Just (M.insert whichPrefix fp x)
            in M.alter doAlter ver curMap
      where
        fpText = either id id $ FP.toText (FP.filename fp)
        -- TODO: for some reason this has to be reversed.
        patTests = reverse [ r | pat <- patterns, r <- match pat fpText ]
    initial = M.empty

{-
  environment variables:

  - KERNEL_TOOL_CLEAN_LIMIT: numbers of kernels to keep
    default=2
  - KERNEL_TOOL_CLEAN_BACKUP_DIR: backup directory.
    default=/boot/backup
 -}

{-
  (TODO)
  `clean` command scans /boot, detects kernel files,
  and limit the number of vaild kernels present to KERNEL_TOOL_CLEAN_LIMIT
  by moving old kernels into KERNEL_TOOL_BACKUP_DIR.

 -}
cmdClean :: IO ()
cmdClean = do
  curEnv <- env
  let kernelNumLimit :: Int
      kernelNumLimit =
        case lookup "KERNEL_TOOL_CLEAN_LIMIT" curEnv of
          Just x
            | [(v,"")] <- reads . T.unpack $ x
              -> if v < 1
                   then error "limit should not be less than one, aborted."
                   else v
          _ -> 2
      backupDir :: FP.FilePath
      backupDir =
        case lookup "KERNEL_TOOL_CLEAN_BACKUP_DIR" curEnv of
          Just p -> FP.fromText p
          Nothing -> "/boot/backup"
  putStrLn $ "Limit number of kernels: " <> show kernelNumLimit
  putStrLn $ "Backup dir: " <> FP.encodeString backupDir
  let kernelFiles = ["vmlinuz", "System.map", "config"]
      setSize = length kernelFiles
  Just m <- reduce Foldl.head $ scanKernelFiles kernelFiles  "/boot"
  let (mFulls, mPartials) = partition ((== setSize). M.size . snd) $ M.toList m
      pprVerSet xs = forM_ xs $ \(k,fileMap) -> do
        putStrLn $ "Kernel Version: " <> T.unpack k
        forM_ (M.toList fileMap) $ \(w, fp) ->
          putStrLn $ "\t" <> T.unpack w <> ": " <> FP.encodeString fp
  putStrLn "Complete kernel versions:"
  pprVerSet mFulls
  putStrLn "Incomplete kernel versions:"
  pprVerSet mPartials

  -- TODO: impl
