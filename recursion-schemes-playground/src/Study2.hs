{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Study2
  ( main
  )
where

import Control.Comonad.Cofree
import Control.Monad
import Data.Foldable
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

fib' :: Nat -> Nat
fib' = histo \case
  Nothing ->
    -- fib 0 is 0
    0
  Just (pre :< Nothing) -> case pre of
    ~0 ->
      {-
        fib 1 is 1,
        note that here we have access to:
        - value of `fib 0`, as LHS of `:<`
        - input of `fib 0`, i.e. `0`, in the form of Nothing.
       -}
      1
  Just (pre1 :< Just (pre2 :< _)) ->
    -- fib n = fib (n-1) + fib (n-2)
    pre1 + pre2

{-
  TODO: this does not seems to be memoizing?
  TODO: if we were to memoize, what should we use as key?
  (as we are dealing with Base ..?)
 -}
catalan :: Nat -> Nat
catalan = histo \case
  Nothing ->
    1
  Just fs ->
    let xs :: [Natural]
        xs = toList fs
        ys = reverse xs
     in sum $ zipWith (*) xs ys

main :: IO ()
main = do
  putStrLn "factorial:"
  forM_ [1 .. 10] \i -> do
    print $ factorial i
  putStrLn "fib:"
  forM_ [1 .. 10] \i -> do
    print $ fib i
  putStrLn "fib':"
  forM_ [1 .. 10] \i -> do
    print $ fib' i
  putStrLn "catalan:"
  forM_ [1 .. 10] \i -> do
    print $ catalan i
  pure ()
