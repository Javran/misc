{-
  An attempt to implement a basic case of Cooley-Tukey FFT algorithm.
 -}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.ST
import Data.Complex

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type Cpx = Complex Double

-- Computes e^((-2 pi i) * (N / k))
expAux :: Int -> Int -> Cpx
expAux k n = cos theta :+ sin theta
  where
    theta = -2 * pi * fromIntegral k / fromIntegral n

splitByParity :: V.Vector Cpx -> (V.Vector Cpx, V.Vector Cpx)
splitByParity vs = (evens, odds)
  where
    l = V.length vs
    oddCount = (l + 1) `quot` 2
    evenCount = l - oddCount
    evens =
      V.fromListN evenCount $ (vs V.!) <$> [0,2..]
    odds =
      V.fromListN oddCount $ (vs V.!) <$> [1,3..]

-- Note: looks like vector length must be a power of two.
ditFft :: V.Vector Cpx -> V.Vector Cpx
ditFft vs
  | V.length vs <= 1 = vs
  | otherwise = runST $ do
    let (es, os) = splitByParity vs
        eResults = ditFft es
        oResults = ditFft os
        l = V.length vs
        half = l `quot` 2
    vec <- VM.unsafeNew l
    forM_ [0 .. half - 1] $ \k -> do
      let a = eResults V.! k
          b = expAux k l * oResults V.! k
      VM.write vec k (a + b)
      VM.write vec (half+k) (a - b)
    V.unsafeFreeze vec

main :: IO ()
main = do
  let cs = zipWith (:+) [0..15] [2,4..]
  print (ditFft (V.fromList cs))
