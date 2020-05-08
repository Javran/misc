{-
  An attempt to implement a basic case of Cooley-Tukey FFT algorithm.
 -}
module Main
  ( main
  ) where

import Data.Complex

import qualified Data.Vector as V

-- Computes e^((-2 pi i) * (N / k))
expAux :: Int -> Int -> Complex Double
expAux k n = cos theta :+ sin theta
  where
    theta = -2 * pi * fromIntegral k / fromIntegral n

main :: IO ()
main = pure ()
