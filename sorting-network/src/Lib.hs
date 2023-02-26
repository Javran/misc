module Lib (
  main,
) where

import Control.Monad

main :: IO ()
main = mapM_ print (gen 8)

{-
  Ref: https://en.wikipedia.org/wiki/Batcher_odd%E2%80%93even_mergesort
 -}
gen n = do
  p <- takeWhile (< n) $ iterate (* 2) 1
  k <- takeWhile (>= 1) $ iterate (\v -> quot v 2) p
  j <-
    let step = 2 * k
        x0 = rem k p
     in [x0, x0 + step .. n - 1 - k]
  i <- [0 .. k - 1]
  let fI = fromIntegral @Int @Double
  guard $ floor @_ @Integer (fI (i + j) / fI (p * 2)) == floor (fI (i + j + k) / fI (p * 2))
  pure (i + j, i + j + k)
