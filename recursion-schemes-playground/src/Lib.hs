{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( main
  )
where

import Control.Comonad.Cofree
import Data.Coerce
import Data.Functor.Foldable
import Data.Monoid
import qualified Study1
import qualified Study2

type Nat = [()]

natFac :: Nat -> Nat
natFac = para $ \case
  -- fac 0 = 1
  Nil -> [()]
  -- fac (N + 1) = (N+1) * fac N {- i.e. previous result -}
  Cons () (u, rOld) -> (() : u) >> rOld

{-
  using para might be an overkill because we are not using all info given.
  but, well, it works.

  now that given definition:

  fib 0 = 0
  fib 1 = 1
  fib (n+2) = fib n + fib (n+1)

  because we demand two previous values, this cannot be done with just one prev value available.
  so instead, consider:

  fibAux n = (fib n, fib (n+1))

  by definition:

  fibAux 0 = (fib 0, fib 1) = (0, 1)
  fibAux (n+1) = (fib (n+1), fib (n+2))
               = (fib (n+1), fib n + fib (n+1))
    note that `fibAux n = (fib n, fib n + fib (n+1))`
    contains all info we need - now it's good enough
    to write an impl using para.

-}
natFib :: Nat -> Nat
natFib = fst . natFibAux
  where
    natFibAux :: Nat -> (Nat, Nat)
    natFibAux = para $ \case
      -- fibAux 0 = (fib 0, fib 1)
      Nil -> ([], [()])
      -- fibAux (N+1) = let (fp0, fp1) = fibAux N in (fp1, fp0 + fp1)
      Cons () (_, (u1, u2)) -> (u2, u1 ++ u2)

-- to show that `para` is indeed an overkill.
-- with the use of natFibAux, we can do the same with `cata`
natFib' :: Nat -> Nat
natFib' = fst . natFibAux
  where
    natFibAux :: Nat -> (Nat, Nat)
    natFibAux = cata $ \case
      -- fibAux 0 = (fib 0, fib 1)
      Nil -> ([], [()])
      -- fibAux (N+1) = let (fp0, fp1) = fibAux N in (fp1, fp0 + fp1)
      Cons () (u, v) -> (v, u ++ v)

natFib'' :: Nat -> Nat
natFib'' = histo $ \case
  -- fib 0 = 0
  Nil -> []
  -- fib 1 = 1
  Cons () (_ :< Nil) -> [()]
  -- fib (n+2) = fib (n+1) + fib n
  Cons () (pre' :< Cons () (pre'' :< _)) -> pre' ++ pre''

{-
  seems "project" is a preparation for do recursion -
  for List, as an example, it provides "Nil" and "Cons"
  so that we can describe the actual computation.
-}

{-
v1 :: ListF Int (Fix (ListF Int))
v1 = case project listF1 of
  Nil -> Nil
  Cons a b -> Cons a b

v2 :: ListF Int [Int]
v2 = case project list1 of
  Nil -> Nil
  Cons a b -> Cons a b
 -}
-- we are basically using the sum monoid
oddSums :: [Int] -> Int
oddSums = prepro odds sumAlg
  where
    -- odds is a natural transformation.
    odds :: forall a. ListF Int a -> ListF Int a
    odds Nil = Nil
    odds v@(Cons h t)
      | odd h = v
      | otherwise = Cons 0 t

    sumAlg Nil = 0
    sumAlg (Cons a b) = a + b

oddSums' :: forall sum. sum ~ Sum Int => [Int] -> Int
oddSums' = getSum . prepro odds sumAlg . coerce @[Int] @[sum]
  where
    odds :: forall a. ListF sum a -> ListF sum a
    odds Nil = Nil
    odds v@(Cons h t)
      | odd (getSum h) = v
      | otherwise = Cons 0 t

    sumAlg Nil = 0
    sumAlg (Cons a b) = a <> b

main1 :: IO ()
main1 = do
  print (length $ natFac (replicate 4 ()))
  print (map (length . natFib . (`replicate` ())) [1 .. 15])
  print (map (length . natFib' . (`replicate` ())) [1 .. 15])
  print (map (length . natFib'' . (`replicate` ())) [1 .. 15])
  print (oddSums [1 .. 15], sum (filter odd [1 .. 15 :: Int]))

main :: IO ()
main = do
  putStrLn "# Study1"
  Study1.main
  putStrLn ""
  putStrLn "# Study2"
  Study2.main
  putStrLn ""
