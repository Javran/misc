{-# LANGUAGE
    FlexibleContexts
  #-}
module Repa where

import Data.Array.Repa
import Data.Functor.Identity
import qualified Graphics.Image as HIP
import qualified Graphics.Image.Interface as HIP
import qualified Graphics.Image.Interface.Repa as HIP

type Arr = Array U DIM2 (HIP.Pixel HIP.Y Double)
type ImageGS = HIP.Image HIP.VS HIP.Y Double
type PixelGS = HIP.Pixel HIP.Y Double

computeCcorr :: ImageGS -> ImageGS -> HIP.Image HIP.RPU HIP.Y Double
computeCcorr img tmpl = result
  where
    result :: HIP.Image HIP.RPU HIP.Y Double
    result = HIP.fromRepaArrayP (computeCcorr' img' tmpl')
    img' = HIP.toRepaArray img
    tmpl' = HIP.toRepaArray tmpl

computeCcorr' :: Arr -> Arr -> Arr
computeCcorr' img tmpl = runIdentity $ computeP dResult
  where
    resultDims = ix2 (imgRows - tmplRows + 1) (imgCols - tmplCols + 1)
    dResult = fromFunction resultDims computePx
      where
        computePx (Z :. r :. c) =
            HIP.PixelY $ computePxNumer coord / sqrt (sumTmpl * sumImg)
          where
            coord = (r,c)
            sumImg = computeSqSum img coord
    Z :. imgRows :. imgCols = extent img
    Z :. tmplRows :. tmplCols = extent tmpl
    computePxNumer :: (Int, Int) -> Double
    computePxNumer (dRow, dCol) =
      Prelude.sum $ do
          r <- [0 .. tmplRows - 1]
          c <- [0 .. tmplCols - 1]
          let imgPx = HIP.getPxC (index img (ix2 (dRow + r) (dCol + c))) HIP.LumaY
          let tmplPx = HIP.getPxC (index tmpl (ix2 r c)) HIP.LumaY
          pure $ imgPx * tmplPx
    sumTmpl = computeSqSum tmpl (0,0)
    computeSqSum imgX (offRow, offCol) = sum $ do
      r <- [0 .. tmplRows - 1]
      c <- [0 .. tmplCols - 1]
      let v = HIP.getPxC (index imgX (ix2 (offRow + r) (offCol + c))) HIP.LumaY
      pure $ v * v
