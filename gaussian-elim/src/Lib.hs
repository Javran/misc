{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad
import Control.Monad.Loops
import Data.Bifunctor
import Data.List
import qualified Puzzle as Pz

data Err i
  = NoMultInv i
  | Underdetermined
  deriving (Show, Eq)

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

-- computes multiplicative inverse modulo p.
-- returns input value on failure.
multInv :: Integral i => i -> i -> Either i i
multInv p x =
  if comm == 1
    then
      Right $
        -- p * s + x * t = 1
        t `mod` p
    else Left x
  where
    (comm, (_s, t)) = extEuclidean p x

mainDemo :: IO ()
mainDemo = do
  let input :: [[Int]]
      input =
        [ [3, 4, 7, 2]
        , [4, 11, 2, 8]
        , [16, 7, 3, 3]
        ]
      [a, b, c] = [4, 15, 7 :: Int]
  2 <- pure $ (a * 3 + b * 4 + c * 7) `rem` 17
  8 <- pure $ (a * 4 + b * 11 + c * 2) `rem` 17
  3 <- pure $ (a * 16 + b * 7 + c * 3) `rem` 17
  putStrLn "input:"
  mapM_ print input
  case solveMat 17 input of
    Right sols -> putStrLn $ "Solution: " <> show sols
    Left (NoMultInv i) ->
      putStrLn $
        "Cannot solve equations as " <> show i <> " does not have a multiplicative inverse."
    Left Underdetermined ->
      putStrLn
        "Cannot solve equations, underdetermined."

solveMat :: Integral i => i -> [[i]] -> Either (Err i) [i]
solveMat m mat = do
  ut <- upperTriangular m mat
  pure $ reverse $ unfoldr (solveStep m) ([], reverse ut)

upperTriangular :: forall i. Integral i => i -> [[i]] -> Either (Err i) [[i]]
upperTriangular m = unfoldrM elimStepM
  where
    elimStepM :: [[i]] -> Either (Err i) (Maybe ([i], [[i]]))
    elimStepM eqns = do
      let alts = do
            -- any equation without a zero on front
            (e@(hd : _), es) <- pick eqns
            guard $ hd /= 0
            pure (e, es)
      case alts of
        [] -> Right Nothing
        ([], _) : _ -> Left Underdetermined
        (e@(hd : _), es) : _ -> do
          invHd <- first NoMultInv $ multInv m hd
          let mul x y = (x * y) `mod` m
              eNorm = fmap (mul invHd) (e :: [i])
              norm eqn@(eh : _) =
                if eh == 0
                  then eqn
                  else zipWith (\a b -> (a - b) `mod` m) eqn (fmap (mul eh) eNorm)
              norm _ = error "length not unique"
          pure $ Just (eNorm, fmap (drop 1 . norm) es)

solveStep :: Integral i => i -> ([i], [[i]]) -> Maybe (i, ([i], [[i]]))
solveStep _ (_, []) = Nothing
solveStep m (xs, hd : tl) = do
  let x = (rhs - sum (zipWith (*) lhs xs)) `mod` m
      1 : ys = hd
      rhs = last ys
      lhs = init ys
  pure (x, (x : xs, tl))

pick :: [a] -> [(a, [a])]
pick xs = map split (init $ zip (inits xs) (tails xs))
  where
    split (ls, v : rs) = (v, ls ++ rs)
    split _ = error "cannot split empty list"

main :: IO ()
main = do
  let mat :: [[Int]]
      mat = map  (Pz.mkRow . Pz.eqn) Pz.coords
  print (solveMat 4 mat)
