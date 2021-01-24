module Javran.MaxFlow.Types
  ( NetworkRep (..)
  , CapacityMap
  , FlowAssignment
  )
where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

{-
  Represents all information of this network, this includes:

  - nrNodeCount, represention number of nodes. the id of all notes range from 1 to nrNodeCount.
  - nrArcCount, which must be the length of nrArcs
  - nrSource / nrSink / nrArcs are obvious, with node id range constraints.

  This is a loose representation in the sense that it doesn't put much restriction on network itself.

 -}
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

{-
  Represents values assigned to all arcs.
  This piece of information must be consistent with the company CapacityMap:

  - an element keyed by (u,v) exists in this map if and only if (u,v) maps to a non-zero capacity
    in the company CapacityMap.

  - the value of all elements must be:

    + non-negative.
    + less or equal to its corresponding max capacity in CapacityMap.

  For a proper flow, extra constrains apply.
  See https://en.wikipedia.org/wiki/Maximum_flow_problem#Definition for details on those constraints.

  This type can also be used to represent preflows.
  See https://en.wikipedia.org/wiki/Push%E2%80%93relabel_maximum_flow_algorithm for what is preflow.
 -}
type FlowAssignment = M.Map (Int, Int) Int
