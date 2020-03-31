{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  #-}
module Main
  ( main
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import Data.Text.Encoding (decodeUtf8)

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL.Lazy as B64L
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.Text as T

data SsrRecord
  = SsrRecord
  { srServer :: BS.ByteString
  , srPort :: Int
  , srProtocol :: BS.ByteString
  , srMethod :: BS.ByteString
  , srObfs :: BS.ByteString
  , srPassword :: BS.ByteString
  , srParams :: [] (BS.ByteString, BS.ByteString)
  } deriving Show
-- ssr://server:port:protocol:method:obfs:password_base64/?params_base64

ssrRecordP :: Atto.Parser SsrRecord
ssrRecordP = do
  srServer <- Atto.takeWhile1 (/= ':') <* ":"
  srPort <- Atto.decimal <* ":"
  srProtocol <- Atto.takeWhile1 (/= ':') <* ":"
  srMethod <- Atto.takeWhile1 (/= ':') <* ":"
  srObfs <- Atto.takeWhile1 (/= ':') <* ":"
  srPassword <- B64.decodeLenient <$> (Atto.takeWhile (/= '/') <* "/?")
  let paramPairP :: Atto.Parser (BS.ByteString, BS.ByteString)
      paramPairP = do
        k <- Atto.takeWhile1 (/= '=') <* "="
        v <- Atto.takeWhile1 (/= '&')
        pure (k, B64.decodeLenient v)
  srParams <- paramPairP `Atto.sepBy'` "&"
  pure SsrRecord {..}

altDecode :: BSL.ByteString -> BSL.ByteString
altDecode = B64L.decodeLenient

processRawSsrLines :: BSL.ByteString -> IO ()
processRawSsrLines raw = do
  putStrLn "++++"
  print raw
  let g@[host,port,protocol,method,obfs,left] =
        BSLC.split ':' . altDecode . BSLC.drop 6 $ raw
  print g
  print (Atto.parseOnly ssrRecordP (BSL.toStrict . altDecode . BSLC.drop 6 $ raw))
  putStrLn "----"

main :: IO ()
main = do
  [addr] <- getArgs
  mgr <- newManager tlsManagerSettings
  req <- parseRequest addr
  resp <- httpLbs req mgr

  let raw = responseBody resp
      rawSsrLines =
        BSLC.lines
        -- lenient mode adds padding for us so we don't have to deal with it.
        . altDecode
        $ raw
  mapM_ processRawSsrLines rawSsrLines
