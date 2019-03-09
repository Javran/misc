{-# LANGUAGE RankNTypes #-}
module Main where

import Data.Aeson
import Data.Maybe

import RawTest (TFunc, raw, g)

val :: Value
val = fromJust (decode raw)

{-

  this is a playground for trying to
  find a solution for constructing tests
  when only the below is known:

  - untyped JSON tests (raw)
  - function's type signature (TFunc)

 -}

{-
  the idea is to have some part automatically generated based on the input
 -}
{- generate start -}
data Testcase =
  Testcase
    (forall r. (Int -> Bool -> String -> [Int] -> [Bool] -> [String] -> r) -> r)
    String

tests :: [Testcase]
tests =
    [ Testcase (\g' -> g' 1 True "a" [1,2,3] [] ["a"]) "1|True|\"a\"|[1,2,3]|[]|[\"a\"]"
    , Testcase (\g' -> g' 20 False "zzz" [1] [False] ["a","z"]) "20|False|\"zzz\"|[1]|[False]|[\"a\",\"z\"]"
    ]
{- generate end -}

runTest :: Int -> Testcase -> TFunc -> IO Bool
runTest ind (Testcase d expected) g = do
  let actual = d g
  if actual == expected
    then pure True
    else do
      putStrLn $ "Test #" <> show ind <>  " failed:"
      putStrLn $ "  Input: " <> show (d (,,,,,))
      putStrLn $ "  Expected: " <> show expected
      putStrLn $ "  Actual: " <> show actual
      pure False

runExperiment :: Int -> Testcase -> TFunc -> IO ()
runExperiment ind (Testcase d expected) g = do
  putStrLn $ "Test #" <> show ind <>  ":"
  putStrLn $ "  Input: " <> show (d (,,,,,))
  putStrLn $ "  Expected: " <> show expected
  putStrLn $ "  Experiment: " <> show (d g)

main :: IO ()
main =
  mapM_ (\(ind,t) -> runTest ind t g) (zip [0..] tests)
