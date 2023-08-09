{-
  This module deals with cropping individual images from spritesmith.
 -}
module Spritesmith where

import Control.Monad
import Control.Monad.Fail
import Data.Aeson
import Data.Tuple
import GHC.Generics
import System.FilePath

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Image as Img

{-
  Example resource:
  - /kcs2/img/common/common_itemicons.json?version=4.5.3.0
  - /kcs2/img/common/common_itemicons.png?version=4.5.3.0

  somehow we need an path for output and we can download all stuff to there.

  also need a dedicate module for https://github.com/twolfson/spritesmith,
  I think we can write one just for PNG format.

  structure of this json file:

  - frames: an object
    - keys are file names, values are objects
      - frame:
        - x,y,w,h
        - rotated: false
        - trimmed: false
        - spriteSourceSize: {x,y,w,h}
        - sourceSize: {w,h}

  example:
  - "common_itemicons_id_75",FrameInfo {fiCoord = (480,80), fiSize = (75,75)}
  - item: 新型砲熕兵装資材
  - examined by gimp (top-left corner is (1,1)), the corresponding icon goes from (481,81) to (555,155).
 -}

data FrameInfo = FrameInfo
  { fiCoord :: (Int, Int) -- (x,y)
  , fiSize :: (Int, Int) -- (w,h)
  }
  deriving (Generic, Show)

instance FromJSON FrameInfo where
  parseJSON = withObject "FrameInfo" $ \v -> do
    rotated <- v .: "rotated"
    when rotated $
      Control.Monad.Fail.fail "rotated shouldn't be False"
    trimmed <- v .: "trimmed"
    when trimmed $
      Control.Monad.Fail.fail "trimmed shouldn't be False"
    (frame :: Object) <- v .: "frame"
    let parseFrame vf = do
          x <- vf .: "x"
          y <- vf .: "y"
          w <- vf .: "w"
          h <- vf .: "h"
          pure $ FrameInfo (x, y) (w, h)
    withObject "FrameInfo.frame" parseFrame (Object frame)

newtype SpriteFrames
  = SpriteFrames (M.Map T.Text FrameInfo)
  deriving (Generic, Show)

instance FromJSON SpriteFrames

data FileMeta = FileMeta
  { fmSize :: (Int, Int) -- (w,h)
  }
  deriving (Show)

instance FromJSON FileMeta where
  parseJSON = withObject "FileMeta" $ \v -> do
    (format :: T.Text) <- v .: "format"
    when (format /= "RGBA8888") $
      Control.Monad.Fail.fail $ "unexpected format: " <> T.unpack format
    (sizeObj :: Object) <- v .: "size"
    w <- sizeObj .: "w"
    h <- sizeObj .: "h"
    pure $ FileMeta (w, h)

newtype FileInfo
  = FileInfo (SpriteFrames, FileMeta)
  deriving (Show)

instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \v -> do
    sf <- v .: "frames"
    fm <- v .: "meta"
    pure $ FileInfo (sf, fm)

extractImage :: Image -> FrameInfo -> Image
extractImage img FrameInfo {fiCoord, fiSize} =
  Img.crop (swap fiCoord) (swap fiSize) img

type Image = Img.Image Img.VS Img.RGBA Img.Word8

loadSpritesmith :: FilePath -> FilePath -> IO (M.Map T.Text Image)
loadSpritesmith jsonFile pngFile = do
  Right (FileInfo (SpriteFrames sf, FileMeta sz)) <-
    eitherDecodeFileStrict @FileInfo jsonFile
  mapM_ print (M.toAscList sf)
  img <- Img.readImageExact' Img.PNG pngFile
  -- img <- Img.readImageRGBA Img.VU pngFile
  -- note that here hip dimension is represented as (h,w), rather than (w,h).
  let (imgH, imgW) = Img.dims img
  when (sz /= (imgW, imgH)) $
    error $ "image size mismatch: " <> show (sz, (imgW, imgH))
  putStrLn $ "width: " <> show imgW <> ", height: " <> show imgH
  let images :: M.Map T.Text Image
      images = M.map (extractImage img) sf
  pure images

outputImages :: FilePath -> M.Map T.Text Image -> IO ()
outputImages outputDir = mapM_ outputImage . M.toList
  where
    outputImage :: (T.Text, Image) -> IO ()
    outputImage (name, img) =
      Img.writeImageExact Img.PNG [] outputFileName img
      where
        outputFileName = outputDir </> T.unpack name <.> "png"
