{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Javran.Gentoo.PackageWatcher.Data.Package
  ( Package (..)
  , safeFromString
  , toText
  )
where

import Data.Aeson
import Data.List.Split (splitOn)
import Data.Maybe
import Data.String
import qualified Data.Text as T

{-
  Represents a full portage package name.
 -}
data Package = Package
  { category :: T.Text
  , name :: T.Text
  }
  deriving (Eq, Ord)

safeFromString :: String -> Maybe Package
safeFromString raw = do
  [T.pack -> category, T.pack -> name] <- pure $ splitOn "/" raw
  pure Package {category, name}

toText :: Package -> T.Text
toText Package {category, name} = category <> "/" <> name

instance IsString Package where
  fromString = fromJust . safeFromString

instance Show Package where
  show = T.unpack . toText

instance FromJSON Package where
  parseJSON = withText "Package" \t -> do
    Just pkg <- pure $ safeFromString $ T.unpack t
    pure pkg

instance ToJSON Package where
  toJSON = String . toText
