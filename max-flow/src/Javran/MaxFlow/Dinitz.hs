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
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
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

buildLayered :: Int -> (Int -> IS.IntSet) -> [Layer]
buildLayered nrSource nextNodes = initLayer : unfoldr expand (srcSet, srcSet)
  where
    srcSet = IS.singleton nrSource
    {-
      - frontVertices are vertices in the current (front) layer, that we need to expand from.
      - discovered are set of vertices that we have discovered (for BFS)
     -}
    expand (frontVertices, discovered) = do
      let expandedLayer :: Layer
          expandedLayer@(vs, _) = foldMap go (IS.toList frontVertices)
            where
              go u =
                let arcs =
                      [ (u, v)
                      | v <- IS.toList (nextNodes u `IS.difference` discovered)
                      ]
                 in (IS.fromList (snd <$> arcs), arcs)

      guard $ not $ IS.null $ vs
      pure (expandedLayer, (vs, discovered <> vs))
    initLayer = (srcSet, [])

buildLayeredM :: M ([Layer], [Layer])
buildLayeredM = do
  (NetworkRep {nrSource, nrSink}, cMap) <- ask
  fl <- get
  let getResidual u v = (\(cur, cap) -> cap - cur) <$> lookupArc cMap fl (u, v)
      nextNodes :: Int -> IS.IntSet
      nextNodes s = IS.filter hasResidual vs
        where
          -- all directly connected vertices except those that has been discovered.
          vs = IS.fromList (IM.keys (cMap IM.! s))
          hasResidual v = case getResidual s v of
            Nothing -> False
            Just r -> r > 0
      layers = buildLayered nrSource nextNodes
      revMap :: IM.IntMap [Int]
      revMap =
        {-
          built from layered network with all arcs reversed.
         -}
        IM.fromListWith (<>) $ do
          (_, ps) <- layers
          (\(u, v) -> (v, [u])) <$> ps
      layers' = buildLayered nrSink (\u -> IS.fromList $ fromMaybe [] (revMap IM.!? u))
  pure (layers, layers')

experiment :: NormalizedNetwork -> IO ()
experiment nn = do
  let (Right ((layers, r), _, _), _) =
        runWriter $ runExceptT $ runRWST buildLayeredM (nr, cMap) initFlow
  putStrLn "layered:"
  mapM_ print (zip [0 ..] layers)
  putStrLn "pruned backwards:"
  mapM_ print (zip [0 ..] r)
  where
    Right (cMap, initFlow) = prepare (getNR nn)
    nr@NetworkRep {} = getNR nn
