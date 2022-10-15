module Lib (
  main,
) where

import qualified Data.ByteString.Lazy as BSL
import Data.String (IsString)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.IO as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (newTlsManager)
import System.Environment
import System.Exit

fileListUri :: IsString s => s
fileListUri = "https://syzygy-tables.info/download.txt?source=lichess&max-pieces=6"

commonPrefix :: IsString s => s
commonPrefix = "https://tablebase.lichess.ovh/tables/"

--- https://aria2.github.io/manual/en/html/aria2c.html#input-file
genInputFileEntity :: FilePath -> T.Text -> [T.Text]
genInputFileEntity targetBase uri =
  [ uri
  , "  dir=" <> T.pack targetBase
  , "  out=" <> targetPath
  ]
  where
    Just targetPath = T.stripPrefix commonPrefix uri

main :: IO ()
main =
  getArgs >>= \case
    ["prepare-list", targetBase] -> do
      mgr <- newTlsManager
      req <- parseRequest fileListUri
      resp <- httpLbs (setRequestCheckStatus req) mgr
      let uris = T.words $ decodeUtf8 $ BSL.toStrict $ responseBody resp
      mapM_ T.putStrLn $ concatMap (genInputFileEntity targetBase) uris
      pure ()
    _ -> die "<prog> prepare-list <target base dir>"
