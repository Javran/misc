{-# LANGUAGE
    RankNTypes
  , GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , LambdaCase
  , TypeApplications
  #-}
module Main where

import Data.List
import Data.Typeable
import Data.Aeson
import Data.Maybe

import RawTest (TFunc, raw, g, typeReps)

data GType a where
  GBool :: GType Bool
  GInt :: GType Int
  GStr :: GType String
  GList :: Typeable a => GType a -> GType [a]

data ETyp = forall a. Typeable a => ETyp (GType a)

ets :: [ETyp]
ets =
  [ ETyp GInt
  , ETyp GBool
  , ETyp GStr
  , ETyp (GList GInt)
  , ETyp (GList GBool)
  , ETyp (GList GStr)
  , ETyp GStr
  ]


val :: [[Value]]
val = fromJust (decode raw)

convert :: forall v. FromJSON v => Value -> GType v -> v
convert jsonVal = \case
    GBool -> get (fromJSON @v jsonVal)
    GInt -> get (fromJSON @v jsonVal)
    GStr -> get (fromJSON @v jsonVal)
    GList _ -> get (fromJSON @v jsonVal)
  where
    get (Success r) = r
    get _ = error "parse error"

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
type Applied r = Int -> Bool -> String -> [Int] -> [Bool] -> [String] -> r
data Testcase =
  Testcase
    (forall r. Applied r -> r)
    String

tests :: [Testcase]
tests =
    [ Testcase (\g' -> g' 1 True "a" [1,2,3] [] ["a"]) "1|True|\"a\"|[1,2,3]|[]|[\"a\"]"
    , Testcase (\g' -> g' 20 False "zzz" [1] [False] ["a","z"]) "20|False|\"zzz\"|[1]|[False]|[\"a\",\"z\"]"
    ]
{- generate end -}

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

haskellModule :: [TypeRep] -> [[Value]] -> String
haskellModule tyReps rawTests = unlines
    [ "{-# LANGUAGE RankNTypes #-}"
    , "module Testcase"
    , "  ( Applied"
    , "  , Testcase(..)"
    , "  , tests"
    , "  ) where"
    , ""
    , "type Applied r = "
      <> intercalate " -> " (map (show . eConvert) argETyps ++ ["r"])
    , ""
    , "data Testcase"
    , "  = Testcase"
    , "      (forall r. Applied r -> r)"
    , "      " <> show (eConvert resultETyp)
    , ""
    , "tests :: [Testcase]"
    , "tests ="
    , "  [" <> intercalate "\n  ," (map mkTestcase rawTests)
    , "  ]"
    ]
  where
    eConvert (ETyp (a :: GType t)) = typeRep (Proxy :: Proxy t)
    mkTestcase :: [Value] -> String
    mkTestcase jsonVals = " Testcase _ _"
      where
    argETyps = init ets
    argLen = length argETyps
    resultETyp = last ets

main :: IO ()
main = do
  putStrLn (haskellModule typeReps val)
  -- mapM_ print val
