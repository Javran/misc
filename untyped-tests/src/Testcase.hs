{-# LANGUAGE RankNTypes #-}
module Testcase
  ( Applied, TResult
  , Testcase(..)
  ) where

import Data.Aeson
import qualified Data.Vector as Vec

type Applied r = Int -> Bool -> [Char] -> [Int] -> [Bool] -> [[Char]] -> r
type TResult = [Char]

data Testcase
  = Testcase
      (forall r. Applied r -> r)
      TResult

showInp :: Testcase -> String
showInp (Testcase d _) = show (d (,,,,,))

instance FromJSON Testcase where
  parseJSON = withArray "Testcase" $ \arr -> do
    v0 <- parseJSON (arr Vec.! 0)
    v1 <- parseJSON (arr Vec.! 1)
    v2 <- parseJSON (arr Vec.! 2)
    v3 <- parseJSON (arr Vec.! 3)
    v4 <- parseJSON (arr Vec.! 4)
    v5 <- parseJSON (arr Vec.! 5)
    r <- parseJSON (arr Vec.! 6)
    pure (Testcase (\g -> g v0 v1 v2 v3 v4 v5) r)

