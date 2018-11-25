{-# LANGUAGE FlexibleContexts, TypeFamilies, LambdaCase #-}
module Main where

import Data.Functor.Foldable

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


natFib :: Nat -> Nat
natFib = fst . natFibAux
  where
    natFibAux :: Nat -> (Nat, Nat)
    natFibAux = para $ \case
        -- fibAux 0 = (fib 0, fib 1)
        Nil -> ([], [()])
        -- fibAux (N+1) = let (fp0, fp1) = fibAux N in (fp1, fp0 + fp1)
        Cons () (_, (u1, u2)) -> (u2, u1++u2)
 
main :: IO ()
main = do
  print listF1
  print list1'
  print list1''
  print listX
  print (length $ natFac (replicate 4 ()))
  print (map (length . natFib . (`replicate` ()) ) [1..15])
