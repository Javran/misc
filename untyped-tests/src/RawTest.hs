module RawTest
  ( TFunc
  , g
  , raw
  ) where

import Data.List
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

type TFunc = Int -> Bool -> String -> [Int] -> [Bool] -> [String] -> String

g :: TFunc
g a b c la lb lc = intercalate "|" [show a, show b, show c, show la, show lb, show lc]

tests :: [(Int, Bool, String, [Int], [Bool], [String], String)]
tests = do
  a <- [0,10,20]
  b <- [False, True]
  c <- words "a b cc dd"
  d <- [[1,2],[3,4]]
  e <- [[], [False], [True, True]]
  f <- [words "a b", words "c d e f"]
  pure (a,b,c,d,e,f,g a b c d e f)

raw :: BSL.ByteString
raw = encode tests
