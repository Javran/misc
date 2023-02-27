module Common where

import Control.Monad
import Data.Bits

{-
  Ref: https://en.wikipedia.org/wiki/Batcher_odd%E2%80%93even_mergesort

  Note: goes out of bound when n is not a power of 2.
  unclear if we can simply just ignore those values?
 -}
gen :: Int -> [] (Int, Int)
gen n = do
  -- INVARIANT: p == shiftL 1 (pw - 1)
  (p, pw) <- zip (takeWhile (< n) $ iterate (* 2) 1) [1 ..]
  k <- takeWhile (>= 1) $ iterate (\v -> shiftR v 1) p
  j <- takeWhile (<= n - 1 - k) $ iterate (+ 2 * k) (rem k p)
  i <- [0 .. k - 1]
  guard $ shiftR (i + j) pw == shiftR (i + j + k) pw
  {-
    Index could get out of bound without this check
    when n is not a power of 2 - not sure about
    its correctness but QuickCheck is yet to find an counterexample.
   -}
  guard $ i + j + k < n
  pure (i + j, i + j + k)
