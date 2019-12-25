{-# LANGUAGE NoMonomorphismRestriction #-}
module Main
  ( main
  ) where

{-
  This is originally a solution that I wrote for https://projecteuler.net/problem=149.
  I'm wondering if we can turn the number generating process into a conduit
  so that we can generate a stream of values for rest of the program
  while keeping just a small buffer.
 -}

import Control.Monad
import Control.Monad.ST
import Data.Int
import Data.Conduit
import Control.Monad.Primitive

import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Conduit.List as CL

result :: Int32
result = findMatrixMax 2000 cells

type Coord = (Int,Int)

{-
  Two vectors of the same length for running Kadane's algorithm.
  - First vector holds current best value.
  - Second vector holds current sum.
 -}
data Kadane v = Kadane v v

{-
  Kadane's algorithm to compute sum of adjacent numbers.
  Note that if the array (or the sequence of things) are all negative,
  the algorithm will return 0, which is fine for our case, because
  there are definitely positive numbers in our array (for example, s_100 = 86613)
 -}
kadaneAux :: (Num a, Ord a) => (a, a) -> a -> (a, a)
kadaneAux (bestSum, prevSum) curVal = (bestSum', curSum)
  where
    curSum = max 0 (prevSum + curVal)
    bestSum' = max bestSum curSum

findMatrixMax :: Int -> [] (Coord, Int32) -> Int32
findMatrixMax l cs = runST $ do
  let initKadane sz = Kadane <$> VUM.new sz <*> VUM.new sz
      updateKadane (Kadane vecBest vecCur) i val = do
        vBest <- VUM.read vecBest i
        vCur <- VUM.read vecCur i
        let (vBest', vCur') = kadaneAux (vBest, vCur) val
        VUM.write vecBest i vBest'
        VUM.write vecCur i vCur'
  rowsKn <- initKadane l
  colsKn <- initKadane l
  diags0Kn <- initKadane (l+l-1)
  diags1Kn <- initKadane (l+l-1)
  let base = l-1 -- this base allows diags1Kn's index to start from 0.
  forM_ cs $ \((r,c), val) -> do
    updateKadane rowsKn r val
    updateKadane colsKn c val
    updateKadane diags0Kn (r+c) val
    updateKadane diags1Kn (r-c+base) val
    pure ()
  let getMax (Kadane mvec _) = do
        vec <- VU.unsafeFreeze mvec
        pure (maximum (VU.toList vec))
  rowsMax <- getMax rowsKn
  colsMax <- getMax colsKn
  diags0Max <- getMax diags0Kn
  diags1Max <- getMax diags1Kn
  pure $ maximum [rowsMax, colsMax, diags0Max, diags1Max]

cells :: [] (Coord, Int32)
cells = zip [ (r,c) | r <- [0..1999], c <- [0..1999] ] vals
  where
    _ : vals = VU.toList numTable

laggedFibGen :: PrimMonad m => ConduitT () Int32 m ()
laggedFibGen = do
    let sz = 128
        fInt = fromIntegral
    vec <- VUM.unsafeNew sz
    -- for 1 <= k <= 55
    forM_ [1..55] $ \k -> do
      let -- being careful here not to overflow.
          v0 = 100003 - 200003 * k
          v1 = modMul (k*k) (modMul k 300007)
          val :: Int32
          val = fInt (modPlus v0 v1 - 500000)
      VUM.write vec k val
      yield val
    forM_ [56..] $ \k -> do
      kM24 <- VUM.read vec ((k-24) `rem` sz)
      kM55 <- VUM.read vec ((k-55) `rem` sz)
      let v0 :: Int
          v0 = modPlus (modPlus (fInt kM24) (fInt kM55)) 1000000
          val :: Int32
          val = fInt (v0 - 500000)
      VUM.write vec (k `rem` sz) val
      yield val
  where
    modPlus a b = (a + b) `rem` m
    modMul a b = (a * b) `rem` m
    m = 1000000
    l = 2000
    sz = 1 + l * l

numTable :: VU.Vector Int32
numTable = runST $ do
    vec <- VUM.unsafeNew sz
    -- for 1 <= k <= 55
    forM_ [1..55] $ \k -> do
      let -- being careful here not to overflow.
          v0 = 100003 - 200003 * k
          v1 = modMul (k*k) (modMul k 300007)
          val = modPlus v0 v1 - 500000
      VUM.write vec k (fromIntegral val)
    forM_ [56..l*l] $ \k -> do
      kM24 <- VUM.read vec (k-24)
      kM55 <- VUM.read vec (k-55)
      let v0 :: Int
          v0 = modPlus (modPlus (fInt kM24) (fInt kM55)) 1000000
          fInt = fromIntegral
          val = v0 - 500000
      VUM.write vec k (fromIntegral val)
    VU.unsafeFreeze vec
  where
    -- `rem` and `mod` produces the same result when m is non-negative,
    -- but rem is slightly more efficient to use.
    modPlus a b = (a + b) `rem` m
    modMul a b = (a * b) `rem` m
    m = 1000000
    l = 2000
    sz = 1 + l * l

main :: IO ()
main = do
  xs <- connect laggedFibGen (CL.take (2000 * 2000))
  let ys = VU.toList (VU.drop 1 numTable)
  print (xs == ys)
