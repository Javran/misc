{-# LANGUAGE
    NamedFieldPuns
  , OverloadedStrings
  , TupleSections
  , ViewPatterns
  #-}
module Main
  ( main
  ) where

import Data.Aeson
import Control.Monad

import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Data.Text as T

data Foo
  = Foo
  { aaa :: Int
  , bbb :: T.Text
  , ccc :: Maybe (Int, Int)
  , extra :: M.Map T.Text T.Text
  }

instance FromJSON Foo where
  parseJSON = withObject "Foo" $ \obj -> do
    aaa <- obj .: "aaa"
    bbb <- obj .: "bbb"
    ccc <- obj .:? "ccc"
    let existingFields = T.words "aaa bbb ccc"
        obj' =
          -- for sake of simplicity, I'm not using the most efficient approach.
          filter ((`notElem` existingFields) . fst)
          . HM.toList
          $ obj
    (M.fromList -> extra) <- forM obj' $ \(k,v) ->
      withText "ExtraText" (pure . (k,)) v
    pure Foo {aaa,bbb,ccc,extra}

main :: IO ()
main = pure ()
