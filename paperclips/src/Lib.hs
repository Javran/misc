module Lib (
  main,
) where

import Control.Monad.RWS.CPS
import Control.Monad.Reader
import qualified Data.Text as T
import Data.Tuple
import qualified Data.Vector.Unboxed as VU
import qualified Data.IntMap as IM
import qualified Data.Vector.Unboxed.Mutable as VUM
import Data.Word
import System.Random.MWC
import System.TimeIt
import Text.Printf

data Move = A | B

other :: Move -> Move
other = \case
  A -> B
  B -> A

{-
  note: pickMove always assume itself to be the "horizontal" one.
 -}
data Strategy = Strategy
  { sName :: T.Text
  , sPickMove :: Payoff -> (Move, Move) -> ReaderT GenIO IO Move
  }

{-
  Should be sufficient for us since max payoff for a tourney is 1600.
 -}
type Score = Word64

type Payoff = (Score, Score, Score, Score) -- aa, ab, ba, bb

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

repeatTourney :: GenIO -> (Move, Move) -> Payoff -> Int -> IO ()
repeatTourney g initPrev p n = do
  v <- VUM.replicate (length strats) (0 :: Score)
  replicateM_ n (runTourney g initPrev p (\i incr -> VUM.modify v (+ incr) i))
  forM_ (zip [0 ..] strats) \(i, s) -> do
    let fI = fromIntegral @Score @Double
    sc <- VUM.read v i
    let ave = fI sc / fromIntegral n
    printf "%20s\t%f\n" (sName s) ave
    pure ()

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
  g <- createSystemRandom
  timeIt do
    repeatTourney g (A, A) (10, 10, 10, 7) 100_000
