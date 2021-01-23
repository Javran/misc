{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Algorithm.Karzanov where

import Control.Monad.Except
import Control.Monad.State
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import qualified Data.Set as S
import Javran.MaxFlow.Algorithm.Internal
import Javran.MaxFlow.Common
import Javran.MaxFlow.Types

{-
  Karzanov's algorithm according to notes:
  "The preflow algorithm for the maximum flow problem"

  TODO: impl
 -}

{-
  represents In(v), which is a stack of (<edge>, <value>)
  this value should be non-negative real but here we are only dealing
  with integers.

  - every node will have an entity in this Map.
 -}
type InStack = IM.IntMap [((Int, Int), Int)]

{-
  represents Out(v): (<first element>, <remaining elements>)

  - the Bool value in first element represents whether it is scanned
  - the first element also represents the active element,
    which means all remaining elements are unscanned.
  - I'm actually not sure why this paper asks for double-linked list specifically,
    but it seems like elements prior to active elements are not used so we can simply drop them here
  - every node will have an entity in this Map unless the list of unscanned Out(v) is empty.

 -}
type OutList = IM.IntMap ((Int, Bool), [Int])

type Frozens = S.Set (Int, Int)

data Extras = Extras
  { eIns :: InStack
  , eOuts :: OutList
  , eFrozens :: Frozens
  }

-- This solver is the standard one plus an extra layer of Extras as StateT
-- moreover, Flow might violate some flow constraints in this module,
-- as we are repurposing that as preflow.
type MK = StateT Extras M

prepare' :: NetworkRep -> Either String (CapacityMap, Flow, Extras)
prepare' nr@NetworkRep {nrSource} = runExcept $ do
  (cMap, fl0) <- liftEither (prepare nr)
  let srcOutArcs :: [((Int, Int), Int)]
      srcOutArcs = do
        (end, cap) <- IM.toList $ IM.filter (> 0) $ cMap IM.! nrSource
        pure ((nrSource, end), cap)
      fl =
        -- set all outgoing edges to its capacity in the preflow.
        M.union (M.fromList srcOutArcs) fl0
      eIns =
        IM.union
          (IM.fromList [(v, [item]) | item@((_, v), _) <- srcOutArcs])
          (IM.map (const []) cMap)
      eOuts = IM.fromList $
        catMaybes $ do
          (u, subMap) <- IM.toList cMap
          let vs :: [Int]
              vs = IM.keys $ IM.filter (> 0) subMap
          (x : xs) <- pure vs
          pure (Just (u, ((x, False), xs)))
  pure (cMap, fl, Extras {eIns, eOuts, eFrozens = mempty})
