{-# LANGUAGE OverloadedStrings, TupleSections #-}
module CommandClean
  ( cmdClean
  ) where

import Control.Monad
import Turtle.Prelude
import Turtle.Shell
import Data.Char
import Turtle.Pattern
import qualified Filesystem.Path.CurrentOS as FP
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Control.Foldl as Foldl
import Control.Applicative


-- scanKernelFiles ["vmlinuz","config","System.map"] "/boot/"
-- return filesets indexed by version
scanKernelFiles :: [T.Text] -> FP.FilePath -> Shell (M.Map T.Text (S.Set T.Text))
scanKernelFiles prefixes bootDir =
    reduce (Foldl.Fold step initial id) $
      ls bootDir >>= \fp -> testfile fp >>= guard >> pure fp
  where
    patterns :: [Pattern (T.Text, T.Text)]
    patterns = toPattern <$> prefixes
      where
        toPattern pref =
          (pref,) <$>
            ( prefix (text pref) *> char '-'
              *> plus (satisfy (not . isSpace)) <* eof
            )
    step curMap fp =
        case patTests of
          [] -> curMap
          (k,v):_ ->
            let doAlter Nothing = Just (S.singleton v)
                doAlter (Just x) = Just (S.insert v x)
            in M.alter doAlter k curMap
      where
        fpText = either id id $ FP.toText fp
        patTests = [ r | pat <- patterns, r <- match pat fpText ]
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
  view $ scanKernelFiles ["vmlinuz", "System.map", "config"] "/boot"
  -- TODO: impl
