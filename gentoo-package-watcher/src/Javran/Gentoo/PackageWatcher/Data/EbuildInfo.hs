{-# LANGUAGE DeriveGeneric #-}

module Javran.Gentoo.PackageWatcher.Data.EbuildInfo where

import Data.Aeson
import GHC.Generics
import Javran.Gentoo.PackageWatcher.Types

data EbuildInfo = EbuildInfo
  { version :: Version
  , extra :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON EbuildInfo

instance ToJSON EbuildInfo
