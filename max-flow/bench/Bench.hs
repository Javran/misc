module Bench
  ( main
  )
where

import Criterion.Main
import Data.Bifunctor
import qualified Data.ByteString as BS
import Data.List
import Data.Text.Encoding (decodeUtf8)
import Javran.MaxFlow.Common
import Javran.MaxFlow.Parser
import Javran.MaxFlow.TestData
import Javran.MaxFlow.Types

pSimple, pGenetic, pHandmade, pRandomBest :: [(String, NormalizedNetwork)]
[ pSimple
  , pGenetic
  , pHandmade
  , pRandomBest
  ] =
    fmap
      normPack
      [ packSimple
      , packGenetic
      , packHandmade
      , packRandomBest
      ]
    where
      normPack :: [(FilePath, BS.ByteString)] -> [(FilePath, NormalizedNetwork)]
      normPack = sortOn fst . fmap (second norm)
        where
          norm :: BS.ByteString -> NormalizedNetwork
          norm raw = normalize nr
            where
              Right nr = parseFromRaw $ decodeUtf8 raw

runSolver :: MaxFlowSolver -> [(String, NormalizedNetwork)] -> [(String, (Int, Flow, CapacityMap))]
runSolver solver =
  (fmap . second) (\nn -> let (Right r, _) = solver nn in r)

main :: IO ()
main = defaultMain []
