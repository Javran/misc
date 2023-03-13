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

main :: IO ()
main = do
  print $ evalCont t1
  print $ evalCont t1Extra
