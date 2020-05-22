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
  -- action for the "outside" applicative.
  tell ["log0"]
  tell ["log1"]
  {-
    action for the "inside" applicative.
    Note that, unlike monad transformer stacks,
    we are intentionally not interleaving actions
    but layering them out: outside and then pure inside.

    This limits each compose action to do only one step,
    but those compose actions can be composed, allowing a limited form
    of interleaving.
   -}
  pure
    (tell [12,34] >> tell [45] :: Writer [Int] ())

main :: IO ()
main = do
  let v = runWriter $ getCompose (wTest *> wTest)
  print v
