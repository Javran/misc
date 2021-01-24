module Javran.MaxFlow.Verify where

import Control.Monad
import Control.Monad.Except
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Monoid
import Javran.MaxFlow.Types

{-
  verify flow property and return total flow.
 -}
verify :: Int -> Int -> CapacityMap -> FlowAssignment -> Either String Int
verify sourceNode sinkNode cMap fl = runExcept $ do
  {-
    verify edges, make sure capacities are within correct ranges,
    and every arc is assigned a capacity (even if it is assigned 0)
   -}
  forM_ (IM.toList cMap) $ \(u, subMap) ->
    forM_ (IM.toList subMap) $ \(v, maxCap) ->
      when (maxCap /= 0) $ do
        when (maxCap < 0) $
          throwError $ "negative capacity on arc " <> show (u, v)
        case fl M.!? (u, v) of
          Just curCap ->
            when (curCap > maxCap) $
              throwError $ "arc " <> show (u, v) <> " exceeds max capacity."
          Nothing ->
            throwError $ "arc " <> show (u, v) <> " is not assigned."
  {-
    According to https://en.wikipedia.org/wiki/Maximum_flow_problem#Definition,
    it is actually unspecfied whether it is allowed for an arc to flow into source or flow out of sink.
    This might just be implied somewhere (by flow definition or by max-flow property), but for now let's
    just verify what definition says and nothing more.
   -}
  let allNodeNets = foldr go (IM.map (const 0) cMap) (M.toList fl)
        where
          incrMap k diff = IM.alter (\(Just x) -> Just (x <> Sum diff)) k
          go ((u, v), c) m =
            if c == 0
              then m
              else incrMap u (- c) . incrMap v c $ m
      srcNet = allNodeNets IM.! sourceNode
      sinkNet = allNodeNets IM.! sinkNode
      nodeNets = IM.delete sourceNode $ IM.delete sinkNode allNodeNets
  when (srcNet + sinkNet /= 0) $
    throwError "unbalanced source and sink net flow."
  when (srcNet > 0) $
    throwError "source node should not receive flow"
  forM_ (IM.toList nodeNets) $ \(u, c) ->
    when (c /= 0) $
      throwError $ "node " <> show u <> " has a non-zero net flow of value " <> show c
  pure $ getSum sinkNet
