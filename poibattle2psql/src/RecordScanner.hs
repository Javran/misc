{-# LANGUAGE OverloadedStrings #-}
module RecordScanner where

import Data.Int
import Data.String
import Prelude hiding (FilePath)
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

import Filesystem.Path.CurrentOS

import qualified Control.Foldl as Foldl
import qualified Prelude (FilePath)
import qualified Data.Text as T

toText' :: FilePath -> T.Text
toText' = either id id . toText

getBattleRecordIds :: Prelude.FilePath -> IO [(Int64, FilePath)]
getBattleRecordIds fpRaw =
  reduce Foldl.list $ do
    fp <- ls (fromString fpRaw)
    let fName = toText' $ filename fp
    [battleId] <- pure $ match (decimal <* ".json.gz") fName
    pure (battleId, fp)
