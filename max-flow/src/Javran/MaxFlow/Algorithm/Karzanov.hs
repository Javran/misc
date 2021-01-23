module Javran.MaxFlow.Algorithm.Karzanov where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S

{-
  Karzanov's algorithm according to notes:
  "The preflow algorithm for the maximum flow problem"

  TODO: impl
 -}

{-
  represents In(v), which is a stack of (<edge>, <value>)
  this value should be non-negative real but here we are only dealing
  with integers.
 -}
type InStack = IM.IntMap [((Int, Int), Int)]

{-
  represents Out(v): (<first element>, <remaining elements>)

  - the Bool value in first element represents whether it is scanned
  - the first element also represents the active element,
    which means all remaining elements are unscanned.
  - I'm actually not sure why this paper asks for double-linked list specifically,
    but it seems like elements prior to active elements are not used so we can simply drop them here
  - to simplify type definition, if Out(v) is empty, it won't be found in this Map.

 -}
type OutList = IM.IntMap ((Int, Bool), [Int])

type Frozens = S.Set (Int, Int)

data Extras = Extras
  { eIns :: InStack
  , eOuts :: OutList
  , eFronzens :: Frozens
  }
