module Main
  ( main
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64.URL.Lazy as B64L
import qualified Data.Text as T

altDecode :: BSL.ByteString -> BSL.ByteString
altDecode = B64L.decodeLenient

processRawSsrLines :: BSL.ByteString -> IO ()
processRawSsrLines raw = do
  putStrLn "++++"
  print raw
  let g@[host,port,protocol,method,obfs,left] =
        BSLC.split ':' . altDecode . BSLC.drop 6 $ raw
  print g
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
