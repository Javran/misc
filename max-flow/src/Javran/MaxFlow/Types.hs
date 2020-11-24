module Javran.MaxFlow.Types
  ( NetworkRep (..)
  , CapacityMap
  , Flow
  )
where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

data NetworkRep = NetworkRep
  { nrNodeCount :: Int
  , nrArcCount :: Int
  , nrSource :: Int
  , nrSink :: Int
  , nrArcs :: [((Int, Int), Int)]
  }
  deriving (Show)

{-
  CapacityMap stores the information about a network and its capacities
  on all arcs.

  This is a map of multiple layers, but we will use short hand (u,v) to indicate
  looking up u in the first layer and u in the second layer.

  Invariants:

  - no self link: there is no element keyed by (u,u) for any u.

  - an element keyed (u,v) exists if and only if (u,v) or (v,u) is in the original network.

    + exactly one of (u,v) or (v,u) is mapped to a capacity of zero and the other one
      capacity of non-zero.

 -}
type CapacityMap = IM.IntMap (IM.IntMap Int)

type Flow = M.Map (Int, Int) Int
