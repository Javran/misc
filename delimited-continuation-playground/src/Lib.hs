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

main :: IO ()
main = print $ evalCont t1
