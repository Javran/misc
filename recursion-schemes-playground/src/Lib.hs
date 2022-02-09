{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Lib
  ( main
  )
where

import Data.Coerce
import Data.Functor.Foldable
import Data.Monoid
import qualified Study1
import qualified Study2

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

_main1 :: IO ()
_main1 = do
  print (oddSums [1 .. 15], sum (filter odd [1 .. 15 :: Int]))
  print (oddSums' [1 .. 15], sum (filter odd [1 .. 15 :: Int]))

{-
  TODO:

  - find example for zygo?
  - references:

    + https://github.com/willtim/recursion-schemes
    + https://stackoverflow.com/q/36851766/315302

 -}

main :: IO ()
main = do
  putStrLn "# Study1"
  Study1.main
  putStrLn ""
  putStrLn "# Study2"
  Study2.main
  putStrLn ""
