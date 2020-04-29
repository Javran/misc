{-# LANGUAGE
    ScopedTypeVariables
  , TypeApplications
  , FlexibleContexts
  #-}
module Main
  ( main
  ) where

import Data.Word
import Data.Foldable
import Data.Ord

import qualified Graphics.Image.Interface as HIP
import qualified Graphics.Image as HIP
import qualified Graphics.Image.IO as HIP

import qualified Repa

type Pixel = HIP.Pixel HIP.RGBA HIP.Word8
type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8
type ImageGS = HIP.Image HIP.VS HIP.Y Double
type PixelGS = HIP.Pixel HIP.Y Double

saturate x
  | x > 1 = 1
  | x < 0 = 0
  | otherwise = x

toGrayscale :: Image -> ImageGS
toGrayscale img = HIP.makeImage imgDims (cv . HIP.index img)
  where
    imgDims = HIP.dims img
    cv :: Pixel -> PixelGS
    cv p =
        -- https://docs.opencv.org/3.4/de/d25/imgproc_color_conversions.html
        HIP.PixelY $ saturate $ (fI r / 255) * 0.299 + (fI g / 255) * 0.587 + (fI b / 255) * 0.114
      where
        fI = fromIntegral @Word8 @Double
        r = HIP.getPxC p HIP.RedRGBA
        g = HIP.getPxC p HIP.GreenRGBA
        b = HIP.getPxC p HIP.BlueRGBA

computeCcorr :: ImageGS -> ImageGS -> HIP.Image HIP.RPU HIP.Y Double
computeCcorr img tmpl = HIP.makeImage resultDims computePx
  where
    computePx :: (Int, Int) -> HIP.Pixel HIP.Y Double
    computePx coord = HIP.PixelY $ computePxNumer coord / sqrt (sumTmpl * sumImg)
      where
        sumTmpl = computeSqSum tmpl (0,0)
        sumImg = computeSqSum img coord
    computePxNumer :: (Int, Int) -> Double
    computePxNumer (dRow, dCol) =
      sum $ do
          r <- [0 .. tmplRows - 1]
          c <- [0 .. tmplCols - 1]
          let imgPx = HIP.getPxC (HIP.index img (dRow + r, dCol + c)) HIP.LumaY
          let tmplPx = HIP.getPxC (HIP.index tmpl (r, c)) HIP.LumaY
          pure $ imgPx * tmplPx
    resultDims = (imgRows - tmplRows + 1, imgCols - tmplCols + 1)
    (imgRows, imgCols) = HIP.dims img
    (tmplRows, tmplCols) = HIP.dims tmpl
    computeSqSum imgX (offRow, offCol) = sum $ do
      r <- [0 .. tmplRows - 1]
      c <- [0 .. tmplCols - 1]
      let v = HIP.getPxC (HIP.index imgX (offRow + r, offCol + c)) HIP.LumaY
      pure $ v * v

main :: IO ()
main = do
  Right (sample :: Image) <- HIP.readImageExact HIP.PNG "sample.png"
  Right (templ :: Image) <- HIP.readImageExact HIP.PNG "templ.png"
  let sampleG = toGrayscale sample
      templG = toGrayscale templ
      computed = computeCcorr sampleG templG
      resultDims = HIP.dims computed
      {-
      maxVal = maximumBy (comparing snd) $ do
        let (rows, cols) = resultDims
        r <- [0 .. rows - 1]
        c <- [0 .. cols - 1]
        let v =  HIP.getPxC (HIP.index computed (r, c)) HIP.LumaY
        pure ((r,c), v) -}
  HIP.displayImage computed
  _ <- getLine
  pure ()
