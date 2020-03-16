{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  , DeriveGeneric
  , ScopedTypeVariables
  #-}
module Spritesmith where

import Data.Aeson
import GHC.Generics
import Control.Monad
import Control.Monad.Fail

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

loadSpritesmith :: FilePath -> FilePath -> IO (FileInfo, Img.Image Img.VS Img.RGBA Img.Word8)
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
  Img.displayImage img
  -- following are just to prevent exiting program too early.
  z <- getLine
  print z
  pure (fi, img)
