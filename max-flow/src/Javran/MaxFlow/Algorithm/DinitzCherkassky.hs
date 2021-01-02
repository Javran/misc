{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Javran.MaxFlow.Algorithm.DinitzCherkassky where

{-
  TODO: Dinitz's algorithm improved by Boris Cherkassky.
 -}

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans.RWS.CPS
import Control.Monad.Trans.Writer.CPS
import Data.Bifunctor
import qualified Data.DList as DL
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Javran.MaxFlow.Algorithm.Dinitz (M, getArc, logM, lookupArc)
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

{-
  Note: phase DFS is a bit tricky to do here, as the algorithm
  requires it to resume at the starting node of first vanishing edge.
  Maybe we can try ListT or even ContT if we want full control of backtracking.

  Additional reading: https://wiki.haskell.org/ListT_done_right

  For whatever reason that I haven't get around to read, ListT in standard
  library is too strict so that extra path are explored rather than stopping
  at the first element available. So we'll probably take a look at list-t package.

  Note: what to do in this case?

  say we've found an augmenting path A -> B -> C -> D,
  and B -> C report vanishing - the expected behavior is
  that DFS backtracks to node B and resuming to try the edge next to B -> C.

  what we can probably do is to let the function return the vanishing node
  and DFS only resumes when vanishing node is the node we are visiting.

  at first glance we need only one path therefore Maybe might be sufficient,
  but then DFS does require that we visit multiple edges from the visiting node
  - therefore for now I'm thinking about ListT.

 -}

{- augment along a path -}
augment :: [Int] -> M Int
augment path = do
  (_, cMap) <- ask
  fl <- get
  let segs :: [((Int, Int), Int)]
      segs =
        zipWith
          (\nFrom nTo -> ((nFrom, nTo), lkup nFrom nTo))
          path
          (tail path)
        where
          lkup u v =
            let Just (val, cap) = lookupArc cMap fl (u, v)
             in cap - val
      pushVal =
        -- value to push along this path
        minimum $ fmap snd segs
      ((btNode, _), _) : _ =
        -- find starting node of the first vanishing edge.
        filter ((== pushVal) . snd) segs
  logM . T.pack $
    "push value: " <> show pushVal
      <> " along path: "
      <> intercalate " -> " (show <$> path)
  when (pushVal <= 0) $ do
    let msg =
          "push value must be positive along this path, while getting "
            <> show pushVal
    logM (T.pack msg)
    lift $ throwError msg
  -- apply flow change
  forM_ segs $ \(arc@(x, y), _) -> do
    (_, cap) <- getArc arc
    modify $
      if cap == 0
        then M.alter (\(Just v) -> Just $ v - pushVal) (y, x)
        else M.alter (\(Just v) -> Just $ v + pushVal) arc
  Control.Monad.Trans.RWS.CPS.tell (Sum pushVal)
  logM $ T.pack $ "report to resume at node " <> show btNode
  pure btNode

phase :: M (Maybe ())
phase = do
  -- TODO: phase to be tested.
  (NetworkRep {nrSink, nrSource}, cMap) <- ask
  initFl <- get
  let ranks = computeRanks cMap initFl nrSink
  if IM.notMember nrSource ranks
    then pure Nothing
    else do
      let dfs :: Int -> Int -> [Int] -> M (Maybe Int)
          dfs curNode curRank revPath = do
            {-
              note that path should be constructed in reversed order
              with curNode as the first element.
             -}
            fl <- get
            if curNode == nrSink
              then
                Just
                  <$>
                  {-
                     augument along this path
                     and return starting point of the first vanishing edge
                     (closer to source)
                   -}
                  augment (reverse revPath)
              else do
                {-
                 visit deeper and examine resulting value to see
                 whether to end the current iteration or keep going.
                -}
                let nextNodes :: [Int]
                    nextNodes = do
                      let nextRank = Just (curRank -1)
                          notFull v = case lookupArc cMap fl (curNode, v) of
                            Nothing -> False
                            Just (cur, cap) -> cap - cur > 0
                      node <-
                        IM.keys
                          . fromMaybe IM.empty
                          $ cMap IM.!? curNode
                      guard $ ranks IM.!? node == nextRank && notFull node
                      pure node
                fix
                  (\loop nodes -> case nodes of
                     [] -> pure Nothing
                     nextNode : nodes' -> do
                       result <- dfs nextNode (ranks IM.! nextNode) (nextNode : revPath)
                       case result of
                         Nothing ->
                           -- keep going if a deeper search finds no result.
                           loop nodes'
                         Just nResume ->
                           if nResume == curNode
                             then loop nodes' -- only resume when we are searching the matching node.
                             else do
                               logM $ T.pack ("abort subsequence searches at node " <> show curNode)
                               pure result)
                  nextNodes
      dfs nrSource (ranks IM.! nrSource) [nrSource]
      pure $ Just ()

phaseUntilFix :: M ()
phaseUntilFix =
  phase >>= \case
    Nothing -> pure ()
    Just () -> phaseUntilFix

experiment :: NormalizedNetwork -> IO ()
experiment nn = do
  let nr@NetworkRep {nrSink} = getNR nn
      Right (cMap, initFlow) = prepare nr
  print $ computeRanks cMap initFlow nrSink
  case runWriter $ runExceptT $ runRWST phaseUntilFix (nr, cMap) initFlow of
    (Right (_, fl, Sum maxVal), ls) -> do
      putStrLn "logs:"
      mapM_ T.putStrLn ls
      putStrLn $ "total value: " <> show maxVal
      putStrLn $ "flow: " <> show fl
    r -> print r

{-
  TODO: experiment and maxFlow might be merged with similar functions found in Dinitz module.
 -}
maxFlow :: MaxFlowSolver
maxFlow (getNR -> nr) = (second (\((), fl, Sum v) -> (v, fl, cMap)) result, DL.toList logs)
  where
    Right (cMap, initFlow) = prepare nr
    (result, logs) =
      runWriter $ runExceptT $ runRWST phaseUntilFix (nr, cMap) initFlow
