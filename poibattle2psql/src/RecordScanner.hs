{-# LANGUAGE OverloadedStrings, TypeApplications #-}
module RecordScanner where

import Control.Exception
import Data.Int
import Data.String
import Filesystem.Path.CurrentOS
import PostgreSQL.Binary.Data
import Prelude hiding (FilePath)
import System.IO hiding (FilePath)
import Turtle.Pattern
import Turtle.Prelude hiding (stderr)
import Turtle.Shell

import qualified Codec.Compression.GZip as GZ
import qualified Control.Foldl as Foldl
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Prelude (FilePath)

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

loadAndDecompress' :: Prelude.FilePath -> IO BSL.ByteString
loadAndDecompress' fp = do
  h <- openFile fp ReadMode
  raw <- BSL.hGetContents h
  -- data for a record is relatively tiny, I'm fine with decompressing it all in-memory.
  let x = BSL.toStrict $ GZ.decompress raw
  x `seq` hClose h
  pure $ BSL.fromStrict x

loadAndDecompress :: Prelude.FilePath -> IO (Either SomeException BSL.ByteString)
loadAndDecompress fp =
  catch @SomeException (Right <$> loadAndDecompress' fp) (pure . Left)

loadBattleRecord :: FilePath -> IO (Either SomeException BattleRecord)
loadBattleRecord fp = error "TODO"
