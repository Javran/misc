module Javran.MaxFlow.Verify where

import Control.Monad
import Control.Monad.Except
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Javran.MaxFlow.Types

verify :: CapacityMap -> Flow -> Except String ()
verify cMap fl = do
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
    TODO: all nodes should have a net-flow of 0 except source and sink.
   -}
