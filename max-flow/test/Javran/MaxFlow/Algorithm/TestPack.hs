{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Algorithm.TestPack where

import Control.Monad
import qualified Data.ByteString as BS
import Data.Either
import Data.Text.Encoding (decodeUtf8)
import Javran.MaxFlow.Common
import Javran.MaxFlow.Parser
import Javran.MaxFlow.TestData
import Javran.MaxFlow.Verify
import Test.Hspec

mkPackSpec :: MaxFlowSolver -> String -> [(FilePath, BS.ByteString)] -> Spec
mkPackSpec runMaxFlow desc pack = do
  describe desc $
    forM_ pack $ \(fPath, raw) -> do
      let Right nrPre = parseFromRaw (decodeUtf8 raw)
          nn = normalize nrPre
          NetworkRep {nrSource, nrSink} = getNR nn
      specify fPath $ do
        let (result, _logs) = runMaxFlow nn
        result `shouldSatisfy` isRight
        let Right (_v, arcs, cMap) = result
        verify nrSource nrSink cMap arcs `shouldSatisfy` isRight

allPackSpec :: MaxFlowSolver -> Spec
allPackSpec runMaxFlow =
  forM_
    [ ("simple", packSimple)
    , ("handmade", packHandmade)
    , ("genetic", packGenetic)
    , ("random-best", packRandomBest)
    ]
    $ \(desc, pack) ->
      mkPackSpec runMaxFlow desc pack
