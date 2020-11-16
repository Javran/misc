{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module EdmondsKarp where

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Types

checkAndBuildCapacity :: NetworkRep -> Either String (M.Map (Int, Int) Int)
checkAndBuildCapacity NetworkRep {nrArcCount, nrArcs, nrNodeCount} = runExcept $ do
  unless (length nrArcs == nrArcCount) $
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
  let allArcs = nrArcs <> fmap revArc nrArcs
        where
          revArc ((src, dst), _) = ((dst, src), 0)
      capa = M.fromList allArcs
  {-
    for each pair (u,v) we expect it to be unique and (v,u) to not be
    a part of the same network. If this expectation is not met,
    an error will be raised.
   -}
  unless (M.size capa == nrArcCount * 2) $
    throwError "capacity map size mismatch"
  pure capa
