{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module Main
  ( main
  ) where

import Dhall

data E2
  = E2
    { u :: Vector Natural
    , t :: Vector Text
    } deriving (Generic, Show)

data Example
  = Example
    { one :: Natural
    , str :: Text
    , nest :: E2
    } deriving (Generic, Show)

instance Interpret E2
instance Interpret Example

main :: IO ()
main = do
  x <- input auto "./confspec.dhall"
  print (x :: Example)
