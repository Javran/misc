{-# LANGUAGE
    ApplicativeDo
  , NamedFieldPuns
  , OverloadedStrings
  , TupleSections
  , ViewPatterns
  #-}
module Main
  ( main
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.Aeson
import Data.Functor.Compose

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

type C = Compose (Writer [String]) (Writer [Int])

wTest :: C ()
wTest = Compose $ do
  tell ["log0"]
  tell ["log1"]
  pure (tell [12,34] >> tell [45] :: Writer [Int] ())

main :: IO ()
main = do
  let v = runWriter $ getCompose wTest
  print v
