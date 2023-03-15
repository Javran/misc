module Lib (
  main,
) where

import Control.Monad.CC

t1 :: CC ans Int
t1 = do
  l <- reset \p -> do
    r' <- shift p \_k ->
      pure (5 * 2)
    pure (3 + r')
  pure (l - 1)

main :: IO ()
main = do
  print $ runCC t1
