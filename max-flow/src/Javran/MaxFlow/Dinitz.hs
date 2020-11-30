module Javran.MaxFlow.Dinitz () where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Javran.MaxFlow.Types

{-
  TODO: implementation of original Dinitz Algorithm as described in:

  http://www.cs.bgu.ac.il/~dinitz/Papers/Dinitz_alg.pdf
 -}

type RInfo = (NetworkRep, CapacityMap)

{-
  Lookup current flow value and capacity of an arc.
 -}
lookupArc :: RInfo -> Flow -> (Int, Int) -> Maybe (Int, Int)
lookupArc (_, cMap) fl p@(u, v) = do
  subCMap <- cMap IM.!? u
  cap <- subCMap IM.!? v
  {-
    direct lookup without fallback.
    constraint on types should be sufficient to ensure that
    this lookup won't fail.
   -}
  let cur =
        if cap == 0
          then - (fl M.! (v, u))
          else fl M.! p
  pure (cur, cap)
