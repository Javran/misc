{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Algorithm.DinitzSpec where

import Javran.MaxFlow.Algorithm.Dinitz
import Javran.MaxFlow.Algorithm.TestPack
import Test.Hspec

spec :: Spec
spec = allPackSpec maxFlow
