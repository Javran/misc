module Main
  ( main
  ) where

import System.Environment
import System.Directory

import Data.List
import Data.Aeson

{-
  For packing poi battle-detail records into one.

  WIP:
  - read a directory and find all ".json.gz" files.
  - put them all into one file, with records merged together
    separated by newline. (the output file)
  - then we can use whatever tool we like to compress that to a smaller one.

 -}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [srcDirRaw, outFile] -> do
      -- get files under srcDirRaw.
      -- this also serves as a sanity check to confirm that this
      -- is actually a directory.
      files <- filter ("json.gz" `isSuffixOf`) <$> listDirectory srcDirRaw
      bOutExist <- doesFileExist outFile
      if bOutExist
        then error $ "File " <> outFile <> " already exists."
        else putStrLn $ "Record count: " <> show (length files)
    _ -> do
      putStrLn "brp <source dir> <output file>"
      pure ()
