module Lib (
  main,
) where

import Control.Monad
import Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Test.QuickCheck

main :: IO ()
main =
  quickCheck $ withMaxSuccess 10000 do
    l <- chooseInt (3, 30)
    -- 0-1 principle
    xs <- replicateM l (chooseEnum (False, True))
    let ys = mySort xs
    pure $ and $ zipWith (<=) ys (tail ys)

mySort :: Ord a => [a] -> [a]
mySort xs = runST do
  v <- V.unsafeThaw (V.fromList xs)
  let n = VM.length v
  forM_ (gen n) \(i, j) -> do
    vI <- VM.unsafeRead v i
    vJ <- VM.unsafeRead v j
    when (vI > vJ) do
      VM.swap v i j

  mapM (VM.unsafeRead v) [0 .. n - 1]

{-
  Ref: https://en.wikipedia.org/wiki/Batcher_odd%E2%80%93even_mergesort

  Note: index goes out of bound when n is not a power of 2.
  unclear if we can simply just ignore those values?
 -}
gen :: Int -> [] (Int, Int)
gen n = do
  p <- takeWhile (< n) $ iterate (* 2) 1
  k <- takeWhile (>= 1) $ iterate (\v -> quot v 2) p
  j <-
    let step = 2 * k
        x0 = rem k p
     in [x0, x0 + step .. n - 1 - k]
  i <- [0 .. k - 1]
  let fI = fromIntegral @Int @Double
  {-
    TODO: the following should hold for non-negative Integers:
    floor (fromIntegral n / fromIntegral (2^k)) === shiftR n k
   -}
  guard $ floor @_ @Integer (fI (i + j) / fI (p * 2)) == floor (fI (i + j + k) / fI (p * 2))
  guard $ i + j + k < n
  pure (i + j, i + j + k)
