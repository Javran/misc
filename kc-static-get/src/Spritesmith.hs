{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , DeriveGeneric
  , ScopedTypeVariables
  , NamedFieldPuns
  #-}
module Spritesmith where

import Control.Monad
import Control.Monad.Fail
import Data.Aeson
import Data.Tuple
import GHC.Generics

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Image as Img

data FrameInfo
  = FrameInfo
  { fiCoord :: (Int, Int) -- (x,y)
  , fiSize :: (Int, Int) -- (w,h)
  } deriving (Generic, Show)

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
          pure $ FrameInfo (x,y) (w,h)
    withObject "FrameInfo.frame" parseFrame (Object frame)

newtype SpriteFrames
  = SpriteFrames (M.Map T.Text FrameInfo)
    deriving (Generic, Show)

instance FromJSON SpriteFrames

data FileMeta
  = FileMeta
  { fmSize :: (Int, Int) -- (w,h)
  } deriving (Show)

instance FromJSON FileMeta where
  parseJSON = withObject "FileMeta" $ \v -> do
    (format :: T.Text) <- v .: "format"
    when (format /= "RGBA8888") $
      Control.Monad.Fail.fail $ "unexpected format: " <> T.unpack format
    (sizeObj :: Object) <- v .: "size"
    w <- sizeObj .: "w"
    h <- sizeObj .: "h"
    pure $ FileMeta (w,h)

newtype FileInfo
  = FileInfo (SpriteFrames, FileMeta)
  deriving (Show)

instance FromJSON FileInfo where
  parseJSON = withObject "FileInfo" $ \v -> do
    sf <- v .: "frames"
    fm <- v .: "meta"
    pure $ FileInfo (sf, fm)

extractImage :: Image -> FrameInfo -> Image
extractImage img FrameInfo{fiCoord, fiSize} = Img.crop (swap fiCoord) (swap fiSize) img

type Image = Img.Image Img.VS Img.RGBA Img.Word8

loadSpritesmith :: FilePath -> FilePath -> IO (FileInfo, Image)
loadSpritesmith jsonFile pngFile = do
  Right fi@(FileInfo (SpriteFrames sf, FileMeta sz)) <- eitherDecodeFileStrict @FileInfo jsonFile
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
  case images M.!? "common_itemicons_id_75" of
    Just curImg -> do
      Img.displayImage curImg
      -- following are just to prevent exiting program too early.
      z <- getLine
      length z `seq` pure ()
    Nothing -> pure ()
  pure (fi, img)
