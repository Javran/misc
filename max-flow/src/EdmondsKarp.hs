{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}

module EdmondsKarp where

import Control.Monad.Except
import Types

checkNetwork :: NetworkRep -> Either String ()
checkNetwork NetworkRep {nrArcCount, nrArcs, nrNodeCount} = runExcept $ do
  unless (length nrArcs == nrArcCount) $
    throwError "arc count mismatched."
  let checkArc ((src, dst), _) = do
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
