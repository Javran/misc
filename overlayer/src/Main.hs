{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  #-}
module Main (main) where

import Turtle hiding (printf,f,g)
import Prelude hiding (FilePath)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Control.Monad
import Text.Printf
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Data.Aeson
import Data.HashMap.Strict as HM
import qualified Data.Text as T

getPoiCachePath :: MonadIO m => m FilePath
getPoiCachePath =
  (\p -> p </> ".config" </> "poi" </> "MyCache" </> "KanColle") <$> home

serverBase :: String
serverBase = "http://203.104.209.134"

getMapUrls :: Int -> Int -> (String, String)
getMapUrls area num = (base <> "_image.png", base <> "_image.json")
  where
    base = printf "%s/kcs2/resources/map/%03d/%02d" serverBase area num

extractFrameInfo :: Int -> Int -> Value -> ((Int, Int), (Int, Int))
extractFrameInfo area num raw
  | Object r <- raw
  , Just (Object framesJ) <- HM.lookup "frames" r
  , Just (Object mapInfoRaw) <- HM.lookup mapInfoKey framesJ
  , Just (Object frameDetail) <- HM.lookup "frame" mapInfoRaw
  = let g k | Just (Number v) <- HM.lookup k frameDetail = round v
        g _ = error "unreachable"
    in ((g "x", g "y"), (g "w", g "h"))
  | otherwise = error "extraction error"
  where
    mapInfoKey = T.pack $ printf "map%03d%02d_map%d-%d" area num area num

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  let area = 5
      num = 2
      (imgUrl, infoUrl) = getMapUrls area num
      simpleReq url = do
        req <- parseUrlThrow url
        responseBody <$> httpLbs req mgr

  imgData <- simpleReq imgUrl
  Just (info :: Value) <- decode <$> simpleReq infoUrl

  print $ BSL.length imgData
  print $ extractFrameInfo area num info
