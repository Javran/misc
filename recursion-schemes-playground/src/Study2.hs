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

factorial :: Natural -> Natural
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

main :: IO ()
main = do
  forM_ [1 .. 10] \i -> do
    print $ factorial i
  pure ()
