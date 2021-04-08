module Lib where

import Control.Monad
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Set as S

input :: [[Int]]
input =
  [ [3, 4, 7, 2]
  , [4, 11, 2, 8]
  , [16, 7, 3, 3]
  ]

-- expect both input to be positive numbers.
extEuclidean :: Integral i => i -> i -> (i, (i, i))
extEuclidean a0 b0 = aux (a0, 1, 0) (b0, 0, 1)
  where
    aux (r0, s0, t0) y@(r1, s1, t1) =
      if r1 == 0
        then (r0, (s0, t0))
        else aux y (r, s0 - q * s1, t0 - q * t1)
      where
        (q, r) = r0 `quotRem` r1

main :: IO ()
main = do
  let [a, b, c] = [4, 15, 7 :: Int]
  2 <- pure $ (a * 3 + b * 4 + c * 7) `rem` 17
  8 <- pure $ (a * 4 + b * 11 + c * 2) `rem` 17
  3 <- pure $ (a * 16 + b * 7 + c * 3) `rem` 17
  let norm n = if n > 0 then n else 17 + n
  print $ map (\x -> (x, norm . snd . snd . extEuclidean 17 $ x)) [1 .. 16]
  let triangle = reverse $ unfoldr (elimStep 17) input
  print triangle
  print $ reverse $ unfoldr (solveStep 17) ([], triangle)

solveStep :: Int -> ([Int], [[Int]]) -> Maybe (Int, ([Int], [[Int]]))
solveStep m (_, []) = Nothing
solveStep m (xs, hd : tl) = do
  let x = (rhs - sum (zipWith (*) lhs xs)) `mod` m
      1 : ys = hd
      rhs = last ys
      lhs = init ys
  pure (x, (x : xs, tl))

elimStep :: Int -> [[Int]] -> Maybe ([Int], [[Int]])
elimStep m eqns = do
  [(a, s)] <- pure $
    take 1 $ do
      (e@(hd : _), es) <- pick eqns
      guard $ hd /= 0
      let (_, (_, invHd')) = extEuclidean m hd
          invHd = invHd' `mod` m
          mul x y = (x * y) `mod` m
          eNorm = fmap (mul invHd) (e :: [Int])
          norm eqn@(eh : _) =
            if eh == 0
              then eqn
              else zipWith (\a b -> (a - b) `mod` m) eqn (fmap (mul eh) eNorm)
          norm _ = error "TODO"
      pure (eNorm, fmap norm es)
  pure (a, fmap (drop 1) s)

pick :: [a] -> [(a, [a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls, v : rs) = (v, ls ++ rs)
    split _ = error "cannot split empty list"
