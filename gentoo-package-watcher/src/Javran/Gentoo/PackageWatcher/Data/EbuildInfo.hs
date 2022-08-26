{-# LANGUAGE DeriveGeneric #-}

module Javran.Gentoo.PackageWatcher.Data.EbuildInfo where

import Data.Aeson
import Javran.Gentoo.PackageWatcher.Types
import GHC.Generics

data EbuildInfo = EbuildInfo
  { version :: Version
  , extra :: Maybe Value
  }
  deriving (Generic, Show)

instance FromJSON EbuildInfo

instance ToJSON EbuildInfo
