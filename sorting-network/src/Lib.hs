module Lib (
  main,
) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import Test.QuickCheck

main :: IO ()
main =
  quickCheck $ withMaxSuccess 10000 do
    l <- chooseInt (3, 22)
    -- 0-1 principle
    xs <- replicateM l (chooseEnum (False, True))
    let ys = mySort xs
    pure $ label ("l=" <> show l) $ and $ zipWith (<=) ys (tail ys)

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

{-
main = print $ three (3, 2, 1)

three :: forall v. Ord v => (v, v, v) -> (v, v, v)
three (a, b, c) =
  sw a b \a b -> sw b c \b c -> sw a b \a b -> (a,b,c)
  where
    sw :: forall r. v -> v -> (v -> v -> r) -> r
    sw u v f = if u <= v then f u v else f v u
 -}
