module Main
  ( main
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment
import Data.Text.Encoding (decodeUtf8)

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Base64.Lazy as B64L
import qualified Data.Text as T

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
  mapM_ print rawSsrLines
