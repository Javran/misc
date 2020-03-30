{-# LANGUAGE OverloadedStrings #-}
module Main
  ( main
  ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment

import qualified Data.ByteString.Base64.Lazy as B64L

main :: IO ()
main = do
  [addr] <- getArgs
  manager <- newManager tlsManagerSettings

  request <- parseRequest addr
  response <- httpLbs request manager

  let raw = responseBody response
  -- one of them must apply.
  print $ B64L.decode raw
  print $ B64L.decode $ raw <> "="
  print $ B64L.decode $ raw <> "=="
