{-# LANGUAGE TemplateHaskell #-}

module Javran.MaxFlow.TestData where

import qualified Data.ByteString as BS
import Data.FileEmbed

packSimple, packGenetic, packHandmade, packRandomBest :: [(FilePath, BS.ByteString)]
packSimple = $(embedDir "embed/simple")
packGenetic = $(embedDir "embed/2015-cec-flows/genetic")
packHandmade = $(embedDir "embed/2015-cec-flows/handmade")
packRandomBest = $(embedDir "embed/2015-cec-flows/random-best")
