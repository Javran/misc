{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , NumericUnderscores
  , ScopedTypeVariables
  , DeriveGeneric
  #-}
module RecordScanner where

import GHC.Generics
import Control.DeepSeq
import Control.Exception
import Data.Aeson
import Data.Int
import Data.String
import Data.Time.Clock.System
import Filesystem.Path.CurrentOS
import PostgreSQL.Binary.Data
import Prelude hiding (FilePath)
import System.IO hiding (FilePath)
import Turtle.Pattern
import Turtle.Prelude hiding (stderr)
import Turtle.Shell

import qualified Codec.Compression.GZip as GZ
import qualified Control.Foldl as Foldl
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

epochMillisecondsToUTCTime :: Int64 -> UTCTime
epochMillisecondsToUTCTime ms = systemToUTCTime st
  where
    (seconds, mills) = ms `quotRem` 1000
    st = MkSystemTime seconds (fromIntegral mills * 1_000_000)

data BattleRecord
  = BattleRecord
  { brId :: Int64
  , brVersion :: T.Text
  , brType :: T.Text
  , brMap :: [Int16]
  , brDesc :: Maybe T.Text
  , brTime :: UTCTime
  , brFleet :: Value
  , brPacket :: [Value]
  } deriving (Generic)

instance NFData BattleRecord

instance FromJSON BattleRecord where
  parseJSON = withObject "BattleRecord" $ \obj -> do
    (t :: Int64) <- obj .: "time"
    BattleRecord
      <$> pure t
      <*> obj .: "version"
      <*> obj .: "type"
      <*> obj .: "map"
      <*> obj .: "desc"
      <*> pure (epochMillisecondsToUTCTime t)
      <*> obj .: "fleet"
      <*> obj .: "packet"

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
