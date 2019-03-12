{-# LANGUAGE
    RankNTypes
  , GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , LambdaCase
  , TypeApplications
  , TypeOperators
  #-}
module Main where

import Control.Monad
import Data.List
import Data.Typeable
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import qualified Data.Vector as Vec

import Testcase
import RawTest (TFunc, raw, g, typeReps)

data GType a where
  GBool :: GType Bool
  GInt :: GType Int
  GStr :: GType String
  GList :: (Typeable a, FromJSON a) => GType a -> GType [a]

eqGType :: GType a -> GType b -> Maybe (a :~: b)
eqGType = \case
  GBool -> \case GBool -> Just Refl; _ -> Nothing
  GInt -> \case GInt -> Just Refl; _ -> Nothing
  GStr -> \case GStr -> Just Refl; _ -> Nothing
  GList n -> \case GList m -> n `eqGType` m >>= \Refl -> Just Refl; _ -> Nothing

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

tests :: [Testcase]
tests = fromJust (decode raw)

showInp :: Testcase -> String
showInp (Testcase d _) = show (d (,,,,,))

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

haskellModule :: String
haskellModule = unlines $
    [ "{-# LANGUAGE RankNTypes #-}"
    , "module Testcase"
    , "  ( Applied, TResult"
    , "  , Testcase(..)"
    , "  ) where"
    , ""
    , "import Data.Aeson"
    , "import qualified Data.Vector as Vec"
    , ""
    , "type Applied r = "
      <> intercalate " -> " (map (show . eConvert) argETyps ++ ["r"])
    , "type TResult = " <> show (eConvert resultETyp)
    , ""
    , "data Testcase"
    , "  = Testcase"
    , "      (forall r. Applied r -> r)"
    , "      TResult"
    , ""
    , "showInp :: Testcase -> String"
    , "showInp (Testcase d _) = show (d (" <> replicate (argLen-1) ',' <> "))"
    , ""
    , "instance FromJSON Testcase where"
    , "  parseJSON = withArray \"Testcase\" $ \\arr -> do"
    ] <>
    map (\s -> "    v" <> show s <> " <- parseJSON (arr Vec.! " <> show s <> ")") argInds
    <>
    [ "    r <- parseJSON (arr Vec.! " <> show argLen <> ")"
    , "    pure (Testcase (\\g -> " <> unwords ("g": map (\i -> 'v':show i) argInds) <> ") r)"
    ]
  where
    argInds = [0..argLen-1]
    eConvert (ETyp (_ :: GType t)) = typeRep (Proxy :: Proxy t)
    argETyps = init ets
    argLen = length argETyps
    resultETyp = last ets

mainRunTest :: IO ()
mainRunTest = do
  xs <- forM (zip [0..] tests) $ \(ind, t) ->
    runTest ind t g
  print (and xs)

mainGen :: IO ()
mainGen = putStrLn haskellModule

main = mainGen
