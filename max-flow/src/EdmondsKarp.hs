{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module EdmondsKarp
  ( prepare
  , experiment
  , maxFlow
  )
where

import Control.Monad.Except
import Control.Monad.Trans.RWS.CPS
import Data.Bifunctor
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import Types

type CapacityMap = IM.IntMap (IM.IntMap Int)

type Flow = M.Map (Int, Int) Int

type M = RWST (NetworkRep, CapacityMap) () Flow (Except String)

type AugPath = ([(Int, Int)], Int)

prepare :: NetworkRep -> Either String (CapacityMap, Flow)
prepare NetworkRep {nrArcCount, nrArcs, nrNodeCount} = runExcept $ do
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
  unless (getSum (foldMap (Sum . IM.size) capa) == nrArcCount * 2) $
    throwError "capacity map size mismatch"
  pure (capa, initFlow)

experiment :: NetworkRep -> Maybe ([(Int, Int)], Int)
experiment nr@NetworkRep {nrSource, nrSink} =
  findAugPath
    nConsts
    nrSource
    nrSink
    initFlow
    IM.empty
    (Seq.singleton nrSource)
  where
    Right (nConsts, initFlow) = prepare nr

findAugPath
  :: CapacityMap
  -> Int
  -> Int
  -> Flow
  -> IM.IntMap (Int, Int)
  -> Seq.Seq Int
  -> Maybe AugPath
findAugPath caps netSrc netDst flow pre q = case Seq.viewl q of
  Seq.EmptyL -> do
    guard $ netDst `IM.member` pre
    let paths = unfoldr go netDst
          where
            go cur = do
              (src, diff) <- pre IM.!? cur
              pure (((src, cur), diff), src)
        flowImp = minimum (fmap snd paths)
    pure (fmap fst paths, flowImp)
  src Seq.:< q' -> do
    subCaps <- caps IM.!? src
    let getFlowCap dst =
          ( if cap > 0
              then flow M.! (src, dst)
              else - (flow M.! (dst, src))
          , cap
          )
          where
            cap = subCaps IM.! dst
        alts :: [(Int, Int)]
        alts = catMaybes $ fmap go (IM.keys subCaps)
          where
            go dst = do
              guard $ dst /= netSrc
              guard $ dst `IM.notMember` pre
              let (fl, cap) = getFlowCap dst
              guard $ cap > fl
              pure (dst, cap - fl)
        pre' :: IM.IntMap (Int, Int)
        pre' =
          -- pre: pairs of (dst, (src, diff)), where diff is cap - fl
          IM.union pre (IM.fromList $ fmap (\(dst, diff) -> (dst, (src, diff))) alts)
        q'' = q' Seq.>< Seq.fromList (fmap fst alts)
    findAugPath caps netSrc netDst flow pre' q''

findAugPathM :: M (Maybe AugPath)
findAugPathM = do
  (NetworkRep {nrSource, nrSink}, cMap) <- ask
  curFlow <- get
  pure $ findAugPath cMap nrSource nrSink curFlow IM.empty (Seq.singleton nrSource)

applyAugPathM :: AugPath -> M ()
applyAugPathM (xs, diff) = mapM_ applyDiff xs
  where
    applyDiff :: (Int, Int) -> M ()
    applyDiff (src, dst) = do
      cMap <- asks snd
      let c = (cMap IM.! src) IM.! dst
      modify $
        if c == 0
          then M.alter (\(Just v) -> Just $ v - diff) (dst, src)
          else M.alter (\(Just v) -> Just $ v + diff) (src, dst)

maxFlow :: NetworkRep -> Either String Flow
maxFlow nr =
  second (\((), flow, ()) -> flow) $
    runExcept $
      runRWST
        (fix $ \loop -> do
           r <- findAugPathM
           case r of
             Nothing -> pure ()
             Just augPath -> applyAugPathM augPath >> loop)
        (nr, nConsts)
        initFlow
  where
    Right (nConsts, initFlow) = prepare nr
