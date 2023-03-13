module Lib (
  main,
) where

import Control.Monad
import Control.Monad.Trans.Cont

t1 :: Cont w Integer
t1 =
  (-)
    <$> reset
      ( fmap
          (3 +)
          (shift (\_ -> pure ((*) 5 2)))
      )
    <*> pure 1

t1Extra :: Cont w Integer
t1Extra =
  (-)
    <$> reset
      ( fmap
          (3 +)
          ( shift
              ( \k -> do
                  {-
                    here k is `3 + {}`.
                    in addition to computing 5*2 inside,
                    we "insert another layer" between
                    reseting point and final result of this,
                    or `8 + {}`
                    resulting in
                    `8 + (3 + {5*2})` being final result for
                    this part of the computation.
                   -}
                  pure $ 8 + k (5 * 2)
              )
          )
      )
    <*> pure 1

t13 :: Cont w String
t13 =
  fst
    <$> reset do
      x <-
        shift
          ( \(_ :: String -> (String, String)) ->
              {-
                as if the whole `reset {}` is simply replaced
                by the expresion below:
               -}
              pure ("hi", "bye")
          )
      {-
        this part is unreachable as shift shortcuts this by
        "dropping the continuation"
       -}
      pure (x, x)

t3 :: forall w. w ~ Int => Cont Int Int
t3 =
  (+)
    <$> reset do
      (2 *) <$> shift \k ->
        {- k = 2 * {} - remember this is scoped up to reset -}
        pure (k (k 10))
    <*> pure 1

-- a stupid version of list reversal, just for demostration.
myRev :: [a] -> Cont [a] [a]
myRev xs = reset
  case xs of
    [] -> pure []
    y : ys -> do
      -- do it recursively
      ys' <- myRev ys
      shift \k -> do
        -- for whatever result we have, append [y] to it.
        pure $ k ys' <> [y]

main :: IO ()
main = do
  print $ evalCont t1
  print $ evalCont t1Extra

  print $ evalCont t13
  print $ evalCont t3

  print $ evalCont $ myRev [1, 2, 3]
