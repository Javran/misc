{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module EdmondsKarp
  ( prepare
  , maxFlow
  )
where

import Control.Monad.Except
import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor
import qualified Data.DList as DL
import qualified Data.IntMap.Strict as IM
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Sequence as Seq
import qualified Data.Text as T
import Types

type CapacityMap = IM.IntMap (IM.IntMap Int)

type Flow = M.Map (Int, Int) Int

{-
  We can certainly extend Sum Int to (DList Text, Sum Int)
  to support logging, which is awkward because every `tell` call will
  then consist of wrapping and unwrapping, with placeholder values (mempty),
  which isn't really ideal.
 -}
type M =
  RWST
    (NetworkRep, CapacityMap)
    (Sum Int)
    Flow
    ( ExceptT
        String
        (Writer (DL.DList T.Text))
    )

type AugPath = ([((Int, Int), Bool)], Int)

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

type PreInfo = (Int, Int, Bool {- whether the actual direction is reversed -})

findAugPath
  :: CapacityMap
  -> Int
  -> Int
  -> Flow
  -> IM.IntMap PreInfo
  -> Seq.Seq Int
  -> Maybe AugPath
findAugPath caps netSrc netDst flow pre q = case Seq.viewl q of
  Seq.EmptyL -> do
    guard $ netDst `IM.member` pre
    let paths = unfoldr go netDst
          where
            go cur = do
              (src, diff, rev) <- pre IM.!? cur
              pure ((((src, cur), rev), diff), src)
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
        alts :: [(Int, Int, Bool)]
        alts = catMaybes $ fmap go (IM.keys subCaps)
          where
            go dst = do
              guard $ dst /= netSrc
              guard $ dst `IM.notMember` pre
              let (fl, cap) = getFlowCap dst
              guard $ cap > fl
              pure (dst, cap - fl, cap == 0)
        pre' :: IM.IntMap PreInfo
        pre' =
          -- pre: pairs of (dst, (src, diff)), where diff is cap - fl
          IM.union pre (IM.fromList $ fmap (\(dst, diff, rev) -> (dst, (src, diff, rev))) alts)
        q'' = q' Seq.>< Seq.fromList (fmap (\(v, _, _) -> v) alts)
    findAugPath caps netSrc netDst flow pre' q''

findAugPathM :: M (Maybe AugPath)
findAugPathM = do
  (NetworkRep {nrSource, nrSink}, cMap) <- ask
  curFlow <- get
  pure $ findAugPath cMap nrSource nrSink curFlow IM.empty (Seq.singleton nrSource)

logM :: T.Text -> M ()
logM t =
  lift $
    lift $
      Control.Monad.Trans.Writer.CPS.tell $
        DL.singleton t

applyAugPathM :: AugPath -> M ()
applyAugPathM (xs, diff) = do
  when (null xs) $ do
    let msg = "error: augmenting path should not be empty"
    logM (T.pack msg)
    lift $ throwError msg
  let ((_, sinkNode),_) : _ = xs
      nodes = reverse (fmap (fst . fst) xs) <> [sinkNode]
      pathVis = intercalate " --> " (fmap show nodes)
  mapM_ applyDiff xs
  Control.Monad.Trans.RWS.CPS.tell $ Sum diff
  logM $
    T.pack "augmenting path: "
      <> T.pack pathVis
      <> ", with capacity "
      <> T.pack (show diff)
  where
    applyDiff :: ((Int, Int), Bool) -> M ()
    applyDiff ((src, dst), rev) = do
      modify $
        if rev
          then M.alter (\(Just v) -> Just $ v - diff) (dst, src)
          else M.alter (\(Just v) -> Just $ v + diff) (src, dst)

maxFlow :: NetworkRep -> (Either String (Int, Flow), [T.Text])
maxFlow nr =
  second DL.toList $
    runWriter $
      fmap (second (\((), flow, Sum v) -> (v, flow))) $
        runExceptT $
          (runRWST
             (fix $ \loop -> do
                r <- findAugPathM
                case r of
                  Nothing -> pure ()
                  Just augPath -> applyAugPathM augPath >> loop)
             (nr, nConsts)
             initFlow)
  where
    Right (nConsts, initFlow) = prepare nr
