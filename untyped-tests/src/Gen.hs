{-# LANGUAGE
    RankNTypes
  , GADTs
  , ExistentialQuantification
  , ScopedTypeVariables
  , TypeOperators
  #-}
module Gen where

import Data.List
import Data.Typeable
import Data.Aeson

data ETyp = forall a. Typeable a => ETyp (GType a)

data GType a where
  GBool :: GType Bool
  GInt :: GType Int
  GStr :: GType String
  GList :: (Typeable a, FromJSON a) => GType a -> GType [a]

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

-- stack build && stack exec -- untyped-gen > src/Testcase.hs
main :: IO ()
main = putStrLn haskellModule
