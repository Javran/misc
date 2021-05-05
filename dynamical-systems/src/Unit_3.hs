module Unit_3
  ( main
  )
where

import Control.Monad
import Text.Printf

logisticIter r = iterate next
  where
    next x = r * x * (1 - x)

logisticSeedCompareIters r x0 y0 =
  zip [0 :: Int ..] $ zip (logisticIter r x0) (logisticIter r y0)

{-
  TODO:

  (a) two different initial condition and diff between them.
  (b) display symbolic sequence, and calculate frequency of LLL, LLR, LRL and so on.
 -}
main :: IO ()
main =
  forM_
    (take 21 $
       logisticSeedCompareIters
         4
         (0.4 :: Double)
         (0.401 :: Double))
    $ \(i, (xi, yi)) -> do
      printf "%d: %.6f %.6f %.6f\n" i xi yi (xi - yi)
