{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
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
import qualified Graphics.Image.IO.Formats as HIP
import qualified Graphics.Image as HIP

getPoiCachePath :: MonadIO m => m FilePath
getPoiCachePath =
  (\p -> p </> ".config" </> "poi" </> "MyCache" </> "KanColle") <$> home

serverBase :: String
serverBase = "http://203.104.209.134"

coords :: [(Int,Int)]
coords =
  [ (601, 261)
  , (796, 261)
  , (994, 261)
  , (703, 499)
  , (899, 499)
  ]

getMapUrls :: Int -> Int -> (String, String)
getMapUrls area num = (base <> "_image.png", base <> "_image.json")
  where
    base = printf "%s/kcs2/resources/map/%03d/%02d" serverBase area num

type Img = HIP.Image HIP.VS HIP.RGBA HIP.Word8

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
  let Right (mapImgData :: Img) =
        HIP.decode HIP.PNG (BSL.toStrict imgData)

  Right (frameData :: Img) <-
    HIP.readImageExact HIP.PNG "frame.png"
  let ((offX, offY),(1200,720)) =  extractFrameInfo area num info
      imposed = foldl imp mapImgData coords
        where
          imp img (cy,cx) =
             HIP.superimpose (x'+2,y'+2) cropped $
              HIP.superimpose xy' frameData img
            where
              xy'@(x',y') = (offX+cx, offY+cy)
              cropped = HIP.crop (x'+2,y'+2) (35-4,141-4) mapImgData

  -- print $ mapImgData
  -- print $ frameData
  -- HIP.displayImage imposed
  print $ extractFrameInfo area num info
  -- _ <- getLine
  HIP.writeImageExact HIP.PNG [] "/tmp/test.png" imposed
