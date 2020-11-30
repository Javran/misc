{-# LANGUAGE NamedFieldPuns #-}

module Javran.MaxFlow.Common
  ( normalize
  , getNR
  , NormalizedNetwork
  )
where

import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
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
