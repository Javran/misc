{-# LANGUAGE
    ScopedTypeVariables
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import Data.Word

import qualified Graphics.Image.Interface as HIP
import qualified Graphics.Image as HIP
import qualified Graphics.Image.IO as HIP

type Pixel = HIP.Pixel HIP.RGBA HIP.Word8
type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8
type ImageGS = HIP.Image HIP.VS HIP.Y Double
type PixelGS = HIP.Pixel HIP.Y Double

toGrayscale :: Image -> ImageGS
toGrayscale img = HIP.makeImage imgDims (cv . HIP.index img)
  where
    imgDims = HIP.dims img
    cv :: Pixel -> PixelGS
    cv p =
        -- https://docs.opencv.org/3.4/de/d25/imgproc_color_conversions.html
        HIP.PixelY $ saturate $ (fI r / 255) * 0.299 + (fI g / 255) * 0.587 + (fI b / 255) * 0.114
      where
        saturate x
          | x > 1 = 1
          | x < 0 = 0
          | otherwise = x
        fI = fromIntegral @Word8 @Double
        r = HIP.getPxC p HIP.RedRGBA
        g = HIP.getPxC p HIP.GreenRGBA
        b = HIP.getPxC p HIP.BlueRGBA

main :: IO ()
main = do
  Right (sample :: Image) <- HIP.readImageExact HIP.PNG "sample.png"
  Right (templ :: Image) <- HIP.readImageExact HIP.PNG "templ.png"
  let sampleG = toGrayscale sample
  HIP.displayImage sampleG
  _ <- getLine
  pure ()
