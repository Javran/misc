{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Study2
  ( main
  )
where

import Control.Monad
import Data.Functor.Foldable
import GHC.Natural

type Nat = Natural

factorial :: Nat -> Nat
factorial = para \case
  Nothing ->
    -- f 0 = 1
    1
  Just (u, uR) ->
    {-
      u: the subtree.
      uR: the result of folding the subtree.

      By definition: f (n+1) = (n+1) * f n

      here we have `n` and `f n` - sufficient to compute `f (n+1)`
     -}
    (u + 1) * uR

{-
  Now try Fibonacci sequence:

  fib 0 = 0
  fib 1 = 1
  fib n = fib (n-1) + fib (n-2) when n >= 2

  Note that we need two previous values to compute the new one.

  However if we try to keep two values around, this becomes possible:

  let fibAux n := (fib n, fib (n+1))

  then by definition:

  fibAux 0
  = (fib 0, fib 1) = (0, 1)

  fibAux (n+1)
  = (fib (n+1), fib (n+2))
  = (fib (n+1), fib n + fib (n+1))

  let (u1, u2) := fibAux n, then u1 = fib n, u2 = fib (n+1)
  we have:

  fibAux (n+1)
  = (u2, u1 + u2)
 -}
fib :: Nat -> Nat
fib = fst . fibAux
  where
    fibAux :: Nat -> (Nat, Nat)
    fibAux = cata \case
      Nothing -> (0, 1)
      Just (u1, u2) -> (u2, u1 + u2)

main :: IO ()
main = do
  putStrLn "factorial:"
  forM_ [1 .. 10] \i -> do
    print $ factorial i
  putStrLn "fib:"
  forM_ [1 .. 10] \i -> do
    print $ fib i
  pure ()
