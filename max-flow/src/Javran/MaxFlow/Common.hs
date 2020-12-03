{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Common
  ( normalize
  , getNR
  , NormalizedNetwork
  , prepare
  )
where

import Control.Monad.Except
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S
import Javran.MaxFlow.Types

newtype NormalizedNetwork = NormalizedNetwork
  { getNR :: NetworkRep
  }

{-
  To normalize a NetworkRep is to remove and combine arcs in it so that:

  - there is no self-link, meaning (u,u) does not exist in the arcs for any u.
  - arcs are combined if their set of source and destionation are shared.
    say ((u,v),c0), ((v,u),c1) is combined into ((u,v),c0-c1) if c0-c1 > 0
  - there is only positive capacity in resulting NetworkRep.

 -}
normalize :: NetworkRep -> NormalizedNetwork
normalize nr@NetworkRep {nrArcs} =
  NormalizedNetwork $
    nr
      { nrArcCount = M.size tmpCapMap
      , nrArcs =
          fmap (\(p@(x, y), Sum v) -> if v > 0 then (p, v) else ((y, x), - v)) $
            M.toList tmpCapMap
      }
  where
    {-
      produce a map whose arcs are normalized (smaller one always go first) and capacity combined.
      the value is non-zero, for a key (x,y), a positive value v means capacity v going from x to y,
      and a negative value v means capacity -v going from y to x.
     -}
    tmpCapMap =
      M.filter (\(Sum x) -> x /= 0) $
        M.fromListWith (<>) $ mapMaybe norm nrArcs
      where
        norm (p@(x, y), v) = case compare x y of
          EQ ->
            {-
              self-link does not carry any capacity.
              (it could, just that it's not very useful)
             -}
            Nothing
          LT -> Just (p, Sum v)
          GT -> Just ((y, x), Sum (- v))

-- TODO: should accept normalized network
prepare :: NetworkRep -> Either String (CapacityMap, Flow)
prepare NetworkRep {nrArcCount, nrArcs, nrNodeCount} = runExcept $ do
  let szArcs = length nrArcs
  unless (szArcs == nrArcCount) $
    throwError "arc count mismatched."
  let checkArc ((src, dst), cap) = do
        {-
          by making sure that capacity is positive in the original graph,
          we can be sure whenever an arc (u,v) with 0 capacity shows up,
          the actual flow goes in the opposite direction.
          (i.e. the actual flow is from v to u with a positive capacity)
         -}
        unless (cap > 0) $
          throwError "capacity must be positive."
        {-
          check each arc and make sure they are not self-linking (src == dst)
          and is within range.
         -}
        when (src == dst) $
          throwError "arc src and dst should not be the same"
        unless (src > 0 && src <= nrNodeCount) $
          throwError "invalid arc src node"
        unless (dst > 0 && dst <= nrNodeCount) $
          throwError "invalid arc src node"
  mapM_ checkArc nrArcs
  let allArcs :: [((Int, Int), Int)]
      allArcs = nrArcs <> fmap revArc nrArcs
        where
          revArc ((src, dst), _) = ((dst, src), 0)
      initFlow = M.fromList $ fmap (\(p, _) -> (p, 0)) nrArcs
      capa :: CapacityMap
      capa = foldr go IM.empty allArcs
        where
          go ((src, dst), cap) =
            IM.alter
              (\case
                 Nothing -> Just $ IM.singleton dst cap
                 Just m -> Just $ IM.insert dst cap m)
              src
  {-
    for each pair (u,v) we expect it to be unique and (v,u) to not be
    a part of the same network. If this expectation is not met,
    an error will be raised.
   -}
  unless (getSum (foldMap (Sum . IM.size) capa) == nrArcCount * 2) $ do
    let pairs = S.fromList $ fmap ((\p@(x, y) -> if x <= y then p else (y, x)) . fst) nrArcs
    throwError $
      "capacity map size mismatch: unique: "
        <> show (S.size pairs)
        <> ", total: "
        <> show szArcs
  pure (capa, initFlow)
