{-
  An attempt to implement a basic case of Cooley-Tukey FFT algorithm.
 -}
{-# LANGUAGE ViewPatterns #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.State.Strict
import Data.Bits
import Data.Complex
import Data.Monoid
import Statistics.Sample
import System.Random
import Text.Printf

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM

type Cpx = Complex Double

-- e^((c pi i) * (k / N))
expAuxCommon :: Int -> Int -> Int -> Cpx
expAuxCommon c k n = cis theta
  where
    theta = pi * fromIntegral (c * k) / fromIntegral n

-- Computes e^((-2 pi i) * (k / N))
expFft :: Int -> Int -> Cpx
expFft = expAuxCommon (-2)

-- Computes e^((2 pi i) * (k / N))
expIfft :: Int -> Int -> Cpx
expIfft = expAuxCommon 2

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

-- TODO: For now we assume that vector length is a power of two,
-- this can be easily extended to any length, by pretending out-of-range
-- values are 0.
gDitFft :: (Int -> Int -> Cpx) -> V.Vector Cpx -> V.Vector Cpx
gDitFft expF = fix $
  \impl vs ->
    let l = V.length vs
    in if l <= 1
      then vs
      else V.create $ do
        let (impl -> es, impl -> os) = splitByParity vs
            hf = l `quot` 2
        vec <- VM.unsafeNew l
        forM_ [0 .. hf - 1] $ \k -> do
          let a = es V.! k
              b = expF k l * os V.! k
          VM.unsafeWrite vec k (a + b)
          VM.unsafeWrite vec (hf + k) (a - b)
        pure vec

ditFft :: V.Vector Cpx -> V.Vector Cpx
ditFft = gDitFft expFft . rightPadZeros

iditFft :: V.Vector Cpx -> V.Vector Cpx
iditFft (rightPadZeros -> vs) = V.map (/ fromIntegral l) (iditFftAux vs)
  where
    l = V.length vs
    iditFftAux :: V.Vector Cpx -> V.Vector Cpx
    iditFftAux = gDitFft expIfft

{-
  right padding zeros in the end of a matrix to make the length of the vector
  a power of 2.

  special case: returns empty vector when the input is empty.
 -}
rightPadZeros :: V.Vector Cpx -> V.Vector Cpx
rightPadZeros vs
    | l <= 1 = vs
    | isPowerOf2 = vs
    | otherwise =
        let closestPowOf2Gt =
              {-
                Find the closest power of two that is greater or equal to input v.
                only works when v > 1 and v is not a power of two.
               -}
              unsafeShiftL 1 (finiteBitSize l - countLeadingZeros l)
        in vs <> V.fromListN (closestPowOf2Gt - l) (repeat 0)
  where
    l = V.length vs
    isPowerOf2 =
      countLeadingZeros l + countTrailingZeros l + 1 == finiteBitSize l

evaluateOnVector :: V.Vector Cpx -> IO ()
evaluateOnVector vs = do
  let result = iditFft (ditFft vs)
      diffs :: V.Vector Double
      diffs = V.zipWith (\x y -> magnitude (x - y)) vs result
  printf "StdDev: %.9f\n" (stdDev diffs)

type M = StateT StdGen IO

genRandomR :: Random a => (a, a) -> M a
genRandomR = state . randomR

genCpxVector :: M (V.Vector Cpx)
genCpxVector =
  genRandomR (1024, 8192) >>= genCpxVectorOfSize

genCpxVectorOfSize :: Int -> M (V.Vector Cpx)
genCpxVectorOfSize l = do
  let rDouble = genRandomR (-100,100)
  xs <- replicateM l $ (:+) <$> rDouble <*> rDouble
  pure $ V.fromListN l xs

evalRandomVector :: M ()
evalRandomVector = do
  vs <- genCpxVector
  liftIO $ do
    printf "Vector length: %d\n" (V.length vs)
    evaluateOnVector vs

directConvolve :: V.Vector Cpx -> V.Vector Cpx -> V.Vector Cpx
directConvolve xs ys = V.fromListN lZ (f <$> [0 .. lZ-1])
  where
    lX = V.length xs
    lY = V.length ys
    lZ = lX + lY - 1
    f i = getSum . foldMap Sum $ do
      j <- [0..i]
      case (xs V.!? j, ys V.!? (i-j)) of
        (Just x, Just y) -> [x * y]
        _ -> []

fftConvolve :: V.Vector Cpx -> V.Vector Cpx -> V.Vector Cpx
fftConvolve xsPre ysPre = V.fromListN lZ (V.toList (iditFft rs))
  where
    lX = V.length xsPre
    lY = V.length ysPre
    lZ = lX + lY - 1
    -- TOOD: admittedly this is a bit wasteful...
    xs = V.fromListN lZ (V.toList xsPre <> repeat 0)
    ys = V.fromListN lZ (V.toList ysPre <> repeat 0)
    xs' = ditFft xs
    ys' = ditFft ys
    rs = V.zipWith (*) xs' ys'

main :: IO ()
main = do
  let cs = (\x -> x*2 :+ (x*2 + 1)) <$> [0..19]
      vs = V.fromList cs
  evaluateOnVector vs
  g <- newStdGen
  evalStateT evalRandomVector g
  let xs = V.fromList [2,3,4,5]
      ys = V.fromList [7,9]
  print (directConvolve xs ys)
  print (fftConvolve xs ys)
