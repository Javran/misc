#!/usr/bin/env stack
-- stack --resolver lts-9.13 --install-ghc runghc --package turtle
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Reflection

tyFunc = reify (\x -> [x+1 :: Int,x+2,x*10])

main :: IO ()
main = print (tyFunc (\p -> reflect p 20))
