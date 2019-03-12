{-# LANGUAGE
    RankNTypes
  , GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , TypeOperators
  #-}
module Main (main) where

import Control.Monad
import Data.Aeson
import Data.Maybe

import Testcase
import RawTest (TFunc, raw, g)

tests :: [Testcase]
tests = fromJust (decode raw)

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

runTest :: Int -> Testcase -> TFunc -> IO Bool
runTest ind (Testcase d expected) g' = do
  let actual = d g'
  if actual == expected
    then pure True
    else do
      putStrLn $ "Test #" <> show ind <>  " failed:"
      putStrLn $ "  Input: " <> show (d (,,,,,))
      putStrLn $ "  Expected: " <> show expected
      putStrLn $ "  Actual: " <> show actual
      pure False

runExperiment :: Int -> Testcase -> TFunc -> IO ()
runExperiment ind (Testcase d expected) g' = do
  putStrLn $ "Test #" <> show ind <>  ":"
  putStrLn $ "  Input: " <> show (d (,,,,,))
  putStrLn $ "  Expected: " <> show expected
  putStrLn $ "  Experiment: " <> show (d g')


main :: IO ()
main = do
  xs <- forM (zip [0..] tests) $ \(ind, t) ->
    runTest ind t g
  print (and xs)
