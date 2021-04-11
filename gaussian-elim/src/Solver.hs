{-# LANGUAGE ScopedTypeVariables #-}

module Solver where

import Control.Monad
import Control.Monad.Loops
import Data.Bifunctor
import Data.List
import Debug.Trace

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

solveMat' :: (Show i, Integral i) => ElimStepM i -> i -> [[i]] -> Either (Err i) [i]
solveMat' fallback m mat = do
  ut <- upperTriangular fallback m mat
  pure $ reverse $ unfoldr (solveStep m) ([], reverse ut)

solveMat :: (Show i, Integral i) => i -> [[i]] -> Either (Err i) [i]
solveMat =
  solveMat'
    (\eqns ->
       {-
               TODO:
               - div by gcd then fill in underdetermined.
               - partition by whether hd is zero
               - Q: but what if hd column are nothing but zero?
                 + need to insert one row with [1 0 0 0 ... 0]
                 + if there are all-zero rows, drop one
                 + otherwise just take diff.
                 + or shuffle a non-zero row to front, solve it and shuffle back?
             -}

       traceShow ("UNDER", eqns) $ Left Underdetermined)

type ElimStepM i = [[i]] -> Either (Err i) (Maybe ([i], [[i]]))

upperTriangular :: forall i. Integral i => ElimStepM i -> i -> [[i]] -> Either (Err i) [[i]]
upperTriangular fallback m = unfoldrM elimStepM
  where
    elimStepM :: ElimStepM i
    elimStepM eqns = do
      let alts = do
            -- any equation without a zero on front
            (e@(hd : _), es) <- pick eqns
            guard $ gcd m hd == 1
            pure (e, es)
      case alts of
        [] ->
          if null eqns
            then Right Nothing
            else fallback eqns
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
