{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Javran.MaxFlow.Algorithm.DinitzCherkassky where

{-
  TODO: Dinitz's algorithm improved by Boris Cherkassky.
 -}

import Control.Monad
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Maybe
import Javran.MaxFlow.Algorithm.Dinitz (lookupArc)
import Javran.MaxFlow.Common
import Javran.MaxFlow.Types

computeRanks :: CapacityMap -> Flow -> Int -> IM.IntMap Int
computeRanks cMap fl dstNode =
  bfs
    (IS.singleton dstNode)
    [(dstNode, 0)]
    (IM.singleton dstNode 0)
  where
    {-
      reversed map for getting ranks.
      despite that edge goes in one direction, CapacityMap has keys
      in both directions so we are not missing any edges here.
     -}
    revMap :: IM.IntMap [Int]
    revMap = IM.fromListWith (<>) $ do
      (u, vs) <- IM.toList cMap
      v <- IM.keys vs
      (cur, cap) <- maybeToList $ lookupArc cMap fl (u, v)
      guard $ cap - cur > 0
      pure (v, [u])

    bfs discovered q acc = case q of
      [] -> acc
      (curNode, rank) : qRem ->
        let nextNodes =
              filter (`IS.notMember` discovered)
                . fromMaybe []
                $ revMap IM.!? curNode
            extras = fmap (,rank + 1) nextNodes
            discovered' = IS.union discovered (IS.fromList nextNodes)
            q' = qRem <> extras
            acc' = IM.union acc (IM.fromList extras)
         in bfs discovered' q' acc'

experiment :: NormalizedNetwork -> IO ()
experiment nn = do
  let nr@NetworkRep {nrSink} = getNR nn
      Right (cMap, fl) = prepare nr
  print $ computeRanks cMap fl nrSink
