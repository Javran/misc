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

exec :: Reifies s F => Proxy s -> IO ()
exec p = print (reflect p 20)

main :: IO ()
main = do
  print (tyFunc $ ($ 20) . reflect)
  -- `reify func id` won't work,
  -- but it seems as long as we use `reify` to "connect"
  -- consumer (exec) with producer (func), it works fine
  reify func exec
