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

{-
  Resulting flow produced by the solver.
  This piece of information must be consistent with the company CapacityMap:

  - an element keyed by (u,v) exists in this map if and only if (u,v) maps to a non-zero capacity
    in the company CapacityMap.

  - the value of all elements must be:

    + non-negative.
    + less or equal to its corresponding max capacity in CapacityMap.

  Other constraints apply, but most of those are not enforced by type system.
  See https://en.wikipedia.org/wiki/Maximum_flow_problem#Definition for details on those constraints.

  (TODO: we will have a verification module to ensure that the max-flow produced by solvers are compliant with
  those constraints)

 -}
type Flow = M.Map (Int, Int) Int
