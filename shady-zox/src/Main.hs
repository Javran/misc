{-# LANGUAGE
    OverloadedStrings
  , RecordWildCards
  #-}
module Main
  ( main
  ) where

import Control.Monad
import Data.Either
import Data.Text.Encoding (decodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment

import qualified Data.Attoparsec.ByteString.Char8 as Atto
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Base64.URL.Lazy as B64L
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T

data SsrRecord
  = SsrRecord
  { srServer :: BS.ByteString
  , srPort :: Int
  , srProtocol :: BS.ByteString
  , srMethod :: BS.ByteString
  , srObfs :: BS.ByteString
  , srPassword :: BS.ByteString
  , srParams :: [] (BS.ByteString, T.Text)
  } deriving Show

ssrRecordP :: Atto.Parser SsrRecord
ssrRecordP = do
  srServer <- Atto.takeWhile1 (/= ':') <* ":"
  srPort <- Atto.decimal <* ":"
  srProtocol <- Atto.takeWhile1 (/= ':') <* ":"
  srMethod <- Atto.takeWhile1 (/= ':') <* ":"
  srObfs <- Atto.takeWhile1 (/= ':') <* ":"
  srPassword <- B64.decodeLenient <$> (Atto.takeWhile (/= '/') <* "/?")
  let paramPairP :: Atto.Parser (BS.ByteString, T.Text)
      paramPairP = do
        k <- Atto.takeWhile1 (/= '=') <* "="
        v <- Atto.takeWhile1 (/= '&')
        pure (k, decodeUtf8 $ B64.decodeLenient v)
  srParams <- paramPairP `Atto.sepBy'` "&"
  pure SsrRecord {..}

processRawSsrLines :: BSL.ByteString -> Either String SsrRecord
processRawSsrLines raw = do
  let (hd, tl) = BSLC.splitAt 6 raw
  unless (hd == "ssr://") $
    Left "Unexpected prefix."
  Atto.parseOnly ssrRecordP (BSL.toStrict . B64L.decodeLenient $ tl)

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
        . B64L.decodeLenient
        $ raw
      (errs, records) = partitionEithers $ fmap processRawSsrLines rawSsrLines
  putStrLn $ "processed: " <> show (length errs + length records)
  putStrLn $ "succeeded: " <> show (length records)
  mapM_ (print . srServer) records
