{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Main where

import Data.Reflection
import Data.Proxy

type F = Int -> [Int]

func :: F
func x = [x+1, x+2, x*10]

-- func moved to type-level and is carried by Proxy
tyFunc :: forall r. (forall s . Reifies s F => Proxy s -> r) -> r
tyFunc = reify func


tyFunc' :: Reifies s F => Proxy s
tyFunc' =
    -- `reify func id` won't work
    reify func $ \Proxy -> Proxy

exec :: Reifies s F => Proxy s -> IO ()
exec p = print (reflect p 20)

asProxyOf :: f s -> Proxy s -> f s
asProxyOf a _ = a

main :: IO ()
main = do
  print (tyFunc $ ($ 20) . reflect)

  -- but it seems as long as we use `reify` to "connect"
  -- consumer (exec) with producer (func), it works fine
  reify func exec

  -- following line doesn't work, GHC has trouble unifying two s variable,
  -- or perhaps it's more accurate to say that only `reify` knows the `s` type
  -- exec (tyFunc' `asProxyOf` Proxy)
