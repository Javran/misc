{-# LANGUAGE FlexibleContexts, TypeFamilies, LambdaCase #-}
module Main where

import Data.Functor.Foldable
import Control.Comonad.Cofree

list1 :: [Int]
list1 = [1,2,4,4,8,10,24]

listCoalg1 :: [Int] -> ListF Int [Int]
listCoalg1 [] = Nil
listCoalg1 (x:xs) = Cons x xs

listAlg1 :: ListF Int [Int] -> [Int]
listAlg1 Nil = []
listAlg1 (Cons a b) = a : b

{-
  for turning list1 into listF1,
  we need to "build up" ListF from a tradictional list,
  which suggests an anamorphism with the original list as seed
-}
listF1 :: Fix (ListF Int)
listF1 = ana listCoalg1 list1

{-
  reconstruction through catamorphism
-}
list1' :: [Int]
list1' = cata listAlg1 listF1

{-
  hylomorphism: construct then tear down
-}
list1'' :: [Int]
list1'' = hylo listAlg1 listCoalg1 list1


lengthF :: (Recursive t, Base t ~ ListF Int) => t -> Int
lengthF = cata coalg
  where
    coalg Nil = 0
    coalg (Cons _ t) = 1 + t

-- an overcomplicated map (*10) for showing catamorphism on regular list
listX :: [Int]
listX = cata f list1
  where
    f :: ListF Int [Int] -> [Int]
    f Nil = []
    f (Cons a b) = (a*10) : b

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
        Cons () (_, (u1, u2)) -> (u2, u1++u2)

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
        Cons () (u,v) -> (v, u ++ v)

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
v1 :: ListF Int (Fix (ListF Int))
v1 = case project listF1 of
  Nil -> Nil
  Cons a b -> Cons a b
v2 :: ListF Int [Int]
v2 = case project list1 of
  Nil -> Nil
  Cons a b -> Cons a b
 
main :: IO ()
main = do
  print listF1
  print list1'
  print list1''
  print listX
  print (length $ natFac (replicate 4 ()))
  print (map (length . natFib . (`replicate` ()) ) [1..15])
  print (map (length . natFib' . (`replicate` ()) ) [1..15])
  print (map (length . natFib'' . (`replicate` ()) ) [1..15])  
