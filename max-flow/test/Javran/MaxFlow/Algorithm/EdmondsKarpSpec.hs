{-# LANGUAGE NamedFieldPuns #-}
module Javran.MaxFlow.Algorithm.EdmondsKarpSpec where

import Javran.MaxFlow.Algorithm.EdmondsKarp
import Javran.MaxFlow.TestData
import Javran.MaxFlow.Verify
import Test.Hspec
import Control.Monad
import Data.Either
import Javran.MaxFlow.Parser
import Data.Text.Encoding (decodeUtf8)
import Javran.MaxFlow.Common

spec :: Spec
spec =
  describe "maxFlow" $
    forM_ packSimple $ \(fPath, raw) -> do
      let Right nrPre = parseFromRaw (decodeUtf8 raw)
          nn = normalize nrPre
          NetworkRep {nrSource, nrSink} = getNR nn
      specify fPath $ do
        let (result, _logs) = maxFlow nn
        result `shouldSatisfy` isRight
        let Right (_v, arcs, cMap) = result
        verify nrSource nrSink cMap arcs `shouldSatisfy` isRight
