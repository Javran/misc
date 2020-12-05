{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Javran.MaxFlow.Dinitz where

import Control.Monad.Except
import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.Writer.CPS
import qualified Data.DList as DL
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Monoid
import qualified Data.Text as T
import Javran.MaxFlow.Common
import Javran.MaxFlow.Types

{-
  TODO: implementation of original Dinitz Algorithm as described in:

  http://www.cs.bgu.ac.il/~dinitz/Papers/Dinitz_alg.pdf
 -}

type RInfo = (NetworkRep, CapacityMap)

type M =
  RWST
    RInfo
    (Sum Int)
    Flow
    ( ExceptT
        String
        (Writer (DL.DList T.Text))
    )

{-
  Lookup current flow value and capacity of an arc.
 -}
lookupArc :: CapacityMap -> Flow -> (Int, Int) -> Maybe (Int, Int)
lookupArc cMap fl p@(u, v) = do
  subCMap <- cMap IM.!? u
  cap <- subCMap IM.!? v
  {-
    direct lookup without fallback.
    constraint on types should be sufficient to ensure that
    this lookup won't fail.
   -}
  let cur =
        if cap == 0
          then - (fl M.! (v, u))
          else fl M.! p
  pure (cur, cap)

logM :: T.Text -> M ()
logM t =
  lift $
    lift $
      Control.Monad.Trans.Writer.CPS.tell $
        DL.singleton t

getArc :: (Int, Int) -> M (Int, Int)
getArc p = do
  cMap <- asks snd
  fl <- get
  case lookupArc cMap fl p of
    Just v -> pure v
    Nothing -> do
      let msg = "lookup failed for edge " <> show p
      logM (T.pack msg)
      lift $ throwError msg

type Layer = (IS.IntSet, [(Int, Int)]) -- (<vertex set, arc set>)

{-
  expandLayer <nextNodes> <eSet>,
  where <nextNodes> gives the set of next nodes and <eSet> is the set of vertices present in current layer.
 -}
expandLayer :: (Int -> IS.IntSet) -> IS.IntSet -> Maybe Layer
expandLayer nextNodes eSet = do
  (nexts :: [(IS.IntSet, [(Int, Int)])]) <- forM (IS.toList eSet) $ \u -> do
    let arcs = [(u, v) | v <- IS.toList (nextNodes u)]
    pure (IS.fromList (snd <$> arcs), arcs)
  let result = mconcat nexts
  guard $ not $ IS.null $ fst result
  pure result

-- TODO: this is constructed but in reversed order.
buildLayeredM :: M ([Layer], IM.IntMap [Int])
buildLayeredM = do
  (NetworkRep {nrSource}, cMap) <- ask
  fl <- get
  let initLayer = (IS.singleton nrSource, [])
  layers <-
    fix
      (\loop eSet discovered layers -> do
         let getResidual u v = (\(cur, cap) -> cap - cur) <$> lookupArc cMap fl (u, v)
             nextNodes :: Int -> IS.IntSet
             nextNodes s = IS.filter hasResidual vs
               where
                 -- all directly connected vertices except those that has been discovered.
                 vs = IS.fromList (IM.keys (cMap IM.! s)) `IS.difference` discovered
                 hasResidual v = case getResidual s v of
                   Nothing -> False
                   Just r -> r > 0
         case expandLayer nextNodes eSet of
           Nothing -> pure layers
           Just nextLayer@(vs, _) -> do
             loop vs (discovered <> vs) (nextLayer : layers))
      (fst initLayer)
      (IS.singleton nrSource)
      [initLayer]
  let revMap :: IM.IntMap [Int]
      revMap = IM.fromListWith (<>) $ fmap (\(u,v) -> (v ,[u])) $ concatMap snd layers
  pure (layers, revMap)

experiment :: NormalizedNetwork -> IO ()
experiment nn = do
  let (Right ((layers, revMap), _, _), _) =
        runWriter $ runExceptT $ runRWST buildLayeredM (nr, cMap) initFlow
  mapM_ print (zip [0 ..] (reverse layers))
  print revMap
  where
    Right (cMap, initFlow) = prepare (getNR nn)
    nr@NetworkRep {} = getNR nn
