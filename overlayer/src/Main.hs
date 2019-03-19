{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , TypeFamilies
  #-}
module Main (main) where

import Control.Exception
import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import Data.HashMap.Strict as HM
import Data.List
import qualified Data.Text as T
import qualified Graphics.Image as HIP
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Prelude hiding (FilePath)
import Text.Printf
import Turtle hiding (printf,f,g,sort,e)

getPoiCachePath :: MonadIO m => m FilePath
getPoiCachePath =
  (\p -> p </> ".config" </> "poi" </> "MyCache" </> "KanColle") <$> home

storeProcessedMap :: MonadIO m => Int -> Int -> Img -> m ()
storeProcessedMap area num img = do
  cachePath <- getPoiCachePath
  let imgDirPath =
        cachePath </> decodeString (printf "kcs2/resources/map/%03d" area)
      imgPath = imgDirPath </> decodeString (printf "%02d_image.png" num)
  mktree imgDirPath
  liftIO $ HIP.writeImageExact HIP.PNG [] (encodeString imgPath) img

serverBase :: String
serverBase = "http://203.104.209.134"

coords :: [(Int,Int)]
coords =
  [ (261, 601)
  , (261, 796)
  , (261, 994)
  , (499, 703)
  , (499, 899)
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
    in -- note the difference:
       -- in HIP x is the vertical axis pointing down from top left corner
       ((g "y", g "x"), (g "w", g "h"))
  | otherwise = error "extraction error"
  where
    mapInfoKey = T.pack $ printf "map%03d%02d_map%d-%d" area num area num

processGameMap :: Manager -> Int -> Int -> IO ()
processGameMap mgr area num = do
  let (imgUrl, infoUrl) = getMapUrls area num
      simpleReq url = do
        req <- parseUrlThrow url
        responseBody <$> httpLbs req mgr

  imgData <- simpleReq imgUrl
  Just (info :: Value) <- decode <$> simpleReq infoUrl
  let Right (mapImgData :: Img) =
        HIP.decode HIP.PNG (BSL.toStrict imgData)
  let frameData =
        HIP.makeImageR HIP.VS (35,141) (const $ HIP.PixelRGBA 0xff 0x00 0xdb 0xff) :: Img
  let ((offX,offY),(1200,720)) = extractFrameInfo area num info
      imposed = foldl imp mapImgData coords
        where
          imp img (cx,cy) =
            HIP.superimpose (x'+2,y'+2) cropped $
              HIP.superimpose (x',y') frameData img
            where
              (x',y') = (offX+cx, offY+cy)
              cropped = HIP.crop (x'+2,y'+2) (35-4,141-4) mapImgData

  storeProcessedMap area num imposed

allGameMaps :: [(Int, Int)]
allGameMaps =
  sort $ [(area,num) | area <- [1..6], num <- [1..5]] <> [(1,6),(7,1),(7,2)]

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  forM_ allGameMaps $ \(area,num) -> do
    printf "Processing %d-%d\n" area num
    catch (processGameMap mgr area num) $ \(e :: SomeException) -> do
      putStr "Exception caught: "
      putStrLn (displayException e)
