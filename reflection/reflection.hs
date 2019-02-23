{-# LANGUAGE FlexibleContexts, RankNTypes, ScopedTypeVariables #-}
module Main where

import Data.Reflection
import Data.Proxy

type F = Int -> [Int]

func :: F
func x = [x+1, x+2, x*10]

-- func moved to type-level and is carried by Proxy
tyFunc :: forall r. (forall s . Reifies s F => Proxy s -> r) -> r
tyFunc = reify func

main :: IO ()
main = print (tyFunc $ ($ 20) . reflect)
