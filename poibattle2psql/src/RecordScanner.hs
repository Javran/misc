{-# LANGUAGE OverloadedStrings #-}
module RecordScanner where

import Control.Exception
import Data.Int
import Data.String
import Filesystem.Path.CurrentOS
import PostgreSQL.Binary.Data
import Prelude hiding (FilePath)
import Turtle.Pattern
import Turtle.Prelude
import Turtle.Shell

import qualified Data.Aeson as Aeson
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

data BattleRecord
  = BattleRecord
  { brId :: Int64
  , brVersion :: T.Text
  , brType :: T.Text
  , brMap :: [Int16]
  , brDesc :: Maybe T.Text
  , brTime :: UTCTime
  , brFleet :: Aeson.Value
  , brPacket :: [Aeson.Value]
  }

loadBattleRecord :: FilePath -> IO (Either SomeException BattleRecord)
loadBattleRecord = error "TODO"
