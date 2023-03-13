module Lib (
  main,
) where

import Control.Monad
import Control.Monad.Trans.Cont

t1 :: Cont w Integer
t1 =
  liftM2
    (-)
    ( reset
        ( liftM2
            (+)
            (pure 3)
            (shift (\k -> liftM2 (*) (pure 5) (pure 2)))
        )
    )
    (pure 1)

main :: IO ()
main = print $ evalCont t1
