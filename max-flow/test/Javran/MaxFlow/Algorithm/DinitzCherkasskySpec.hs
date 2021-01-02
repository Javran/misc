{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Algorithm.DinitzCherkasskySpec where

import Javran.MaxFlow.Algorithm.DinitzCherkassky
import Javran.MaxFlow.Algorithm.TestPack
import Test.Hspec

spec :: Spec
spec = allPackSpec maxFlow
