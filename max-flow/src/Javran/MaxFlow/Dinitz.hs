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
import Data.Ord
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Tuple
import Javran.MaxFlow.Common
import Javran.MaxFlow.Types

{-
  Implementation of the original Dinitz Algorithm as described in:

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

showM :: Show a => a -> M ()
showM = logM . T.pack . show

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

type Layer' = (IS.IntSet, IM.IntMap [Int]) -- (<vertex set, arc set>)

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

buildLayeredM :: M PLN
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
      pruned :: IM.IntMap [Int]
      pruned = IM.fromListWith (<>) $ do
        {-
          since in a layered network, archs always move forward to next layer of nodes,
          merging all of them into a single bundle of archs does not change its correctness.
          (or in other words, the algorithm is still correct but now the concept of layer is implicit.)
         -}
        (_, es) <- layers'
        (v, u) <- es
        pure (u, [v])
  logM "layered:"
  mapM_ showM (zip [0 :: Int ..] layers)
  logM "pruned network:"
  showM pruned
  pure pruned

{-
  It is assumed that `path` is non-empty and contains source as first element and sink as last one.
 -}
flowChange :: [Int] -> M [(Int, Int)]
flowChange path = do
  let arcs = zip path (tail path)
  arcsAndResiduals <- forM arcs $ \arc -> do
    (fl, cap) <- getArc arc
    logM . T.pack $ show arc <> ": " <> show fl <> "/" <> show cap <> " (" <> show (cap - fl) <> ")"
    pure (arc, cap - fl)
  let (_, val) = minimumBy (comparing snd) arcsAndResiduals
      saturated = [arc | (arc, r) <- arcsAndResiduals, r == val]
  forM_ arcs $ \arc@(x, y) -> do
    (_, cap) <- getArc arc
    modify $
      if cap == 0
        then M.alter (\(Just v) -> Just $ v - val) (y, x)
        else M.alter (\(Just v) -> Just $ v + val) arc
  Control.Monad.Trans.RWS.CPS.tell (Sum val)
  logM $
    "push value: " <> T.pack (show val)
      <> ", saturated: "
      <> T.pack (intercalate ", " (fmap show saturated))
  pure saturated

{-
  pruned layered network.
  all entities should have non-empty list as value and
  no duplicated elements in that list.
 -}
type PLN = IM.IntMap [Int]

-- find and push flow change based on pruned layered network.
augment :: PLN -> M (Maybe PLN)
augment g = do
  (NetworkRep {nrSource, nrSink}, _) <- ask
  let path =
        -- note that return value is non-empty as starting point is always included.
        findPath nrSource g
  if last path == nrSink
    then do
      logM $ "path: " <> T.intercalate " -> " (fmap (T.pack . show) path)
      sat <- flowChange path
      let g1 = foldr removeArc g sat
      g2 <- rightPass g1 sat
      g3 <- leftPass g2 sat
      pure (Just g3)
    else do
      logM "no path found."
      -- report vanishing
      pure Nothing

findPath :: Int -> PLN -> [Int]
findPath srcNode g = srcNode : unfoldr go srcNode
  where
    go curNode = do
      (next : _) <- g IM.!? curNode
      pure (next, next)

-- remove an arc from the graph, and ensure no entity has an empty list as value.
removeArc :: (Int, Int) -> PLN -> PLN
removeArc (u, v) = IM.alter alt u
  where
    alt Nothing =
      Nothing
    alt (Just xs) = do
      xs'@(_ : _) <- pure $ delete v xs
      pure xs'

rightPass :: PLN -> [(Int, Int)] -> M PLN
rightPass initLyd initQ = do
  let -- build up a reverse map for RightPass
      initRevLyd :: PLN
      initRevLyd = IM.fromListWith (<>) $ do
        (u, vs) <- IM.toList initLyd
        v <- vs
        pure (v, [u])
  fix
    (\loop q lyd revLyd -> case q of
       [] -> pure lyd
       (_u, v) : q' -> case revLyd IM.!? v of
         Just _ -> loop q' lyd revLyd
         Nothing -> do
           -- v has no incoming edges, we remove all its outgoing edges and enqueue them.
           let removeArcs = fromMaybe [] $ do
                 ys <- lyd IM.!? v
                 pure [(v, y) | y <- ys]
               lyd' = foldr removeArc lyd removeArcs
               revLyd' = foldr (\p m -> removeArc (swap p) m) revLyd removeArcs
           unless (null removeArcs) $
             logM $ "RightPass: remove arcs: " <> T.pack (intercalate "," (fmap show removeArcs))
           loop (q' <> removeArcs) lyd' revLyd')
    initQ
    initLyd
    initRevLyd

leftPass :: PLN -> [(Int, Int)] -> M PLN
leftPass initLyd initQ = do
  let -- build up a reverse map for LeftPass
      initRevLyd :: PLN
      initRevLyd = IM.fromListWith (<>) $ do
        (u, vs) <- IM.toList initLyd
        v <- vs
        pure (v, [u])
  fix
    (\loop q lyd revLyd -> case q of
       [] -> pure lyd
       (u, _v) : q' -> case lyd IM.!? u of
         Just _ -> loop q' lyd revLyd
         Nothing -> do
           -- u has no outgoing edges, we remove all its incoming edges and enqueue them.
           let removeArcs = fromMaybe [] $ do
                 ys <- revLyd IM.!? u
                 pure [(y, u) | y <- ys]
               lyd' = foldr removeArc lyd removeArcs
               revLyd' = foldr (\p m -> removeArc (swap p) m) revLyd removeArcs
           unless (null removeArcs) $
             logM $ "LeftPass: remove arcs: " <> T.pack (intercalate "," (fmap show removeArcs))
           loop (q' <> removeArcs) lyd' revLyd')
    initQ
    initLyd
    initRevLyd

runPhaseM :: M (Maybe ())
runPhaseM = do
  logM $ "New phase."
  initLyd <- buildLayeredM
  if IM.null initLyd
    then do
      logM $ "Pruned layered network empty, done."
      pure Nothing
    else
      fix
        (\loop curLyd -> do
           r' <- augment curLyd
           case r' of
             Nothing -> do
               logM $ "Layered network vanished."
               pure $ Just ()
             Just nextLyd -> loop nextLyd)
        initLyd

maxFlowM :: M ()
maxFlowM = do
  r <- runPhaseM
  case r of
    Nothing -> pure ()
    Just () -> maxFlowM

experiment :: NormalizedNetwork -> IO ()
experiment nn = do
  let (Right ((), fl, Sum maxVal), ls) =
        runWriter $ runExceptT $ runRWST maxFlowM (nr, cMap) initFlow
  putStrLn "logs:"
  mapM_ T.putStrLn ls
  putStrLn $ "total value: " <> show maxVal
  putStrLn $ "flow: " <> show fl
  where
    Right (cMap, initFlow) = prepare (getNR nn)
    nr@NetworkRep {} = getNR nn
