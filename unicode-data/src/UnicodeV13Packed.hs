{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module UnicodeV13Packed
  ( generalCategory
  , isLetter
  , isMark
  , isNumber
  , isPunctuation
  , isSymbol
  , isSeparator
  , gcDb
  )
where

import ConstructDatabase
import Data.Binary
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.FileEmbed
import Data.Functor.Contravariant
import qualified GeneralCategoryPredicates as GCP

raw :: BS.ByteString
raw = $(embedFile "embed/v13.0.0-packed.raw")

gcDb :: PackedGCDatabase
gcDb = decode $ BSL.fromStrict raw

GCP.GeneralCategoryPredicates {..} =
  contramap (queryPacked gcDb) GCP.predicates
