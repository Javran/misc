{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Javran.MaxFlow.Algorithm.Karzanov where

import Control.Monad.Except
import Control.Monad.State hiding (get)
import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.Writer.CPS
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Javran.MaxFlow.Algorithm.DinitzCherkassky (computeRanks)
import Javran.MaxFlow.Algorithm.Internal
import Javran.MaxFlow.Common
import Javran.MaxFlow.Types

{-
  Karzanov's algorithm according to notes:
  "The preflow algorithm for the maximum flow problem"

  TODO: impl

  The algorithm runs in phases.

  For each phase:

  - rank of nodes are computed by doing BFS from sink to source,
    if source isn't assigned a rank, the algorithm stops.
  - prepare the initial preflow.
  - alternative between "pushing" and "balancing" until this phase is done
  - then proceed to next phase.

  Note: the method requires re-labeling nodes to v_1, v_2, ... so that arc `v_i -> v_j` implies `i < j`.
  This same effect can be achieved by computing a rank from sink (and walking backwards), and
  test that `v_x -> v_y` iff. `rank(v_x) == rank(v_y) + 1`.

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
  deriving (Show)

-- This solver is the standard one plus an extra layer of Extras as StateT
-- moreover, Flow might violate some flow constraints in this module,
-- as we are repurposing that as preflow.
type MK = StateT Extras M

{-
  TODO:
  Note that we are not reading the original paper but an extension of it -
  the preflow algorithm is meant to be applied to a layered network, or more generally
  any acyclic network, which is not generally the case - maybe run a relabeling by BFS
  gives us such an acyclic network, but I'd like to take a step back and think carefully.
 -}

prepare' :: NetworkRep -> Either String (CapacityMap, FlowAssignment, Extras)
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

phase :: M (Maybe ())
phase = do
  (NetworkRep {nrSource, nrSink}, cMap) <- ask
  fl <- get
  let ranks = computeRanks cMap fl nrSink
  case ranks IM.!? nrSource of
    Nothing -> pure Nothing
    Just _ -> do
      let nextVs :: Int -> [Int]
          nextVs u = do
            -- computes all available arcs from a specific node in the layered network.
            let rankU = ranks IM.! u
            v <- IM.keys $ cMap IM.! u
            guard $ ranks IM.!? v == Just (rankU -1)
            (val, cap) <- maybeToList $ lookupArc cMap fl (u, v)
            guard $ cap - val > 0
            pure v
      logM "Layered network:"
      {-
        Print out the network layer by layer and build up list of vertices.
       -}
      vertices <-
        fix
          (\loop q discovered acc -> case q of
             [] -> pure acc
             v : q' -> do
               let vs = nextVs v
                   nexts = filter (`S.notMember` discovered) vs
               logM $ T.pack (show v) <> ": " <> T.pack (show vs)
               loop
                 (q' <> nexts)
                 (S.union discovered (S.fromList vs))
                 (acc <> Seq.fromList nexts))
          [nrSource]
          (S.singleton nrSource)
          (Seq.singleton nrSource)
      {-
        resulting vertices are stored in Seq so that traversal and reverse traversal are fast.
        the algorithm calls for doubly linked list so that "push" phase can access vertices in traversal
        and "balance" phase can access vertices in reverse traversal.
       -}
      logM $ "vertices: " <> (T.pack (show vertices))
      -- TODO: initialize preflow and compute extra state value
      {-
        TODO: the algorithm requires relabeling,
        but it seems the only use for this subscription is to ensure a certain order
        and the actual number doesn't matter. so here we can put together a list of nodes
        with ranks in descending order (since rank is the distance to the sink)
        (note that this also excludes unreachable nodes from this layered network),
        which should hold sufficient information for the algorithm to proceed.
       -}
      pure Nothing

solve :: M ()
solve =
  phase >>= \case
    Nothing -> pure ()
    Just () -> solve

experiment :: NormalizedNetwork -> IO ()
experiment = debugRun solve
