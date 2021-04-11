{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Control.Monad
import Control.Monad.Loops
import Data.Bifunctor
import Data.List
import Data.List.Split
import Debug.Trace
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

solveMat :: (Show i, Integral i) => i -> [[i]] -> Either (Err i) [i]
solveMat m mat = do
  ut <- upperTriangular m mat
  pure $ reverse $ unfoldr (solveStep m) ([], reverse ut)

upperTriangular :: forall i. (Show i, Integral i) => i -> [[i]] -> Either (Err i) [[i]]
upperTriangular m = unfoldrM elimStepM
  where
    elimStepM :: [[i]] -> Either (Err i) (Maybe ([i], [[i]]))
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
            else {-
                    TODO:
                    - div by gcd then fill in underdetermined.
                    - partition by whether hd is zero
                    - Q: but what if hd column are nothing but zero?
                      + need to insert one row with [1 0 0 0 ... 0]
                      + if there are all-zero rows, drop one
                      + otherwise just take diff.
                      + or shuffle a non-zero row to front, solve it and shuffle back?
                  -}
              Right (traceShow ("UNDER", eqns) Nothing)
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
      mat = map (Pz.mkRow . Pz.eqn) Pz.coords
      Right r = solveMat 6 Pz.hexExample
      Right z = upperTriangular 6 Pz.hexExample
      u =
        [ [1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        , [1, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3]
        , [1, 0, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5]
        , [1, 1, 0, 5, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3]
        , [1, 1, 5, 1, 1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 0, 5, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3]
        , [1, 0, 0, 0, 0, 1, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1]
        , [1, 5, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5]
        , [1, 0, 5, 0, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 1, 5, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 5]
        , [1, 5, 0, 0, 5, 0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 5, 5, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
        , [1, 0, 5, 0, 1, 5, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 1, 1, 5, 2, 0, 0, 0, 5, 5, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 1, 1, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]
        , [1, 2, 2, 2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3]
        , [1, 2, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]
        , [1, 1, 1, 1, 0, 5, 5, 5, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 4]
        , [1, 0, 5, 5, 5, 5, 0, 0, 0, 5, 5, 0, 0, 0, 0, 0, 0, 0, 1]
        , [1, 1, 2, 1, 1, 1, 1, 0, 2, 2, 0, 0, 0, 0, 0, 0, 0, 4]
        , [1, 0, 3, 4, 2, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2]
        , [1, 5, 3, 0, 5, 5, 2, 5, 3, 0, 0, 0, 0, 0, 0, 2]
        , [1, 4, 1, 2, 1, 5, 3, 3, 0, 0, 0, 0, 0, 0, 0]
        , [1, 1, 4, 2, 4, 1, 4, 5, 0, 0, 0, 0, 0, 2]
        , [1, 1, 0, 0, 0, 1, 1, 1, 0, 0, 1, 1, 0]
        , [1, 2, 3, 0, 2, 4, 2, 0, 0, 3, 3, 3]
        , [1, 1, 0, 4, 5, 0, 0, 0, 4, 4, 0]
        , [1, 1, 0, 0, 0, 1, 1, 0, 0, 3]
        , [1, 1, 0, 0, 1, 1, 1, 0, 2]
        , [1, 5, 1, 0, 1, 2, 5, 4]
        , [1, 1, 0, 0, 1, 1, 3]
        , [1, 0, 0, 1, 0, 1]
        , [1, 0, 0, 0, 0]
        , [1, 0, 0, 0]
        , [1, 0, 0]
        , [1, 0]
        ]

  -- print (solveMat 4 mat)
  -- Pz.main
  -- mapM_ print (zipWith (\pd xs -> replicate pd 0 <> xs) [0..] z)
  print z
  let sol = reverse $ unfoldr (solveStep 6) ([], reverse u)
  mapM_ print (splitPlaces [4 :: Int, 5, 6, 7, 6, 5, 4] sol)
  pure ()
