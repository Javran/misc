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
  print (oddSums [1 .. 15], sum (filter odd [1 .. 15 :: Int]))

main :: IO ()
main = do
  putStrLn "# Study1"
  Study1.main
  putStrLn ""
  putStrLn "# Study2"
  Study2.main
  putStrLn ""
