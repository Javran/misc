module Lib (
  main,
) where

import Control.Concurrent.Async
import Control.Monad.RWS.CPS
import Control.Monad.Reader
import Data.List
import Data.Ord
import Data.Tuple
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import System.Random.MWC
import System.TimeIt
import qualified Data.Map.Strict as M
import Text.Printf
import Types
import SimResult
import qualified Data.Aeson as A

biggestPayoff :: Payoff -> (Move, Move)
biggestPayoff (aa, ab, ba, bb)
  | aa == best = (A, A)
  | ab == best = (A, B)
  | ba == best = (B, A)
  | otherwise = (B, B)
  where
    best = max aa (max ab (max ba bb))

strats :: [Strategy]
strats =
  [ Strategy "RANDOM" \_ _ -> do
      g <- ask
      b <- uniformM g
      pure if b then A else B
  , Strategy "A100" \_ _ -> pure A
  , Strategy "B100" \_ _ -> pure B
  , Strategy "GREEDY" \p _ -> pure $ fst $ biggestPayoff p
  , Strategy "GENEROUS" \p _ -> pure $ snd $ biggestPayoff p
  , Strategy "MINIMAX" \p _ -> pure $ other $ snd $ biggestPayoff p
  , Strategy "TIT FOR TAT" \_ (_, oppo) -> pure oppo
  , Strategy "BEAT LAST" \(aa, ab, ba, bb) (_, oppo) -> pure case oppo of
      A -> if aa > ba then A else B
      B -> if ab > bb then A else B
  ]

{-
  The original code have this messy way of generating pairings that
  it's probably best that we retain this behavior - it does matter as
  some strategies rely on knowing previous moves.
 -}
pairings :: [((Int, Strategy), (Int, Strategy))]
pairings =
  [(sHd, v) | v <- ss]
  <> [(h, v) | h <- sTl, v <- ss']
  where
    ss = zip [0 ..] strats
    ~(sHd : sTl) = ss
    ss' = sTl <> [sHd]

runTourney :: GenIO -> (Move, Move) -> Payoff -> (Int -> Score -> IO ()) -> IO ()
runTourney g initPrev p@(aa, ab, ba, bb) applyPayoff = do
  let pairings' = concatMap (replicate 10) pairings
      evalRound prev ((indH, sH), (indV, sV)) = do
        hm <- sPickMove sH p prev
        vm <- sPickMove sV p (swap prev)
        let (payH, payV) = case (hm, vm) of
              (A, A) -> (aa, aa)
              (A, B) -> (ab, ba)
              (B, A) -> (ba, ab)
              (B, B) -> (bb, bb)
        lift do
          applyPayoff indH payH
          applyPayoff indV payV
        pure (hm, vm)
  _ <- runReaderT (foldM evalRound initPrev pairings') g
  pure ()

repeatTourney :: GenIO -> (Move, Move) -> Payoff -> Int -> IO [Score]
repeatTourney g initPrev p n = do
  v <- VUM.replicate (length strats) (0 :: Score)
  replicateM_ n (runTourney g initPrev p (\i incr -> VUM.modify v (+ incr) i))
  VU.toList <$> VU.unsafeFreeze v

pprResult :: Int -> [Score] -> IO ()
pprResult n rs = do
  let sorted = sortOn (\(i, sc) -> (Down sc, i)) $ zip [0 :: Int ..] rs
  forM_ sorted \(i, sc) -> do
    let s = strats !! i
    let ave :: Double
        ave = fromIntegral sc / fromIntegral n
    printf "%20s\t%f\n" (sName s) ave

{-
speed with unboxed vector:

- 216.13s
- 162.72s
- 121.76s
- 132.98s
- 204.27s

 -}
main :: IO ()
main = do
  let m = 128
      n = 1024
      simulateFor :: Payoff -> IO [Score]
      simulateFor p = do
        scores <- replicateConcurrently m do
          g <- createSystemRandom
          repeatTourney g (A, A) p n
        pure $ foldr1 (zipWith (+)) scores
      space = do
        aa <- [0 .. 10]
        ab <- [0 .. 10]
        ba <- [0 .. 10]
        bb <- [0 .. 10]
        pure (aa, ab, ba, bb)

  rs <- mapConcurrently
    ( \p -> do
        sc <- simulateFor p
        pure (p, (m * n, sc))
    )
    space
  let results :: SimResults
      results = M.fromList rs
  A.encodeFile "sim-results.json" results
