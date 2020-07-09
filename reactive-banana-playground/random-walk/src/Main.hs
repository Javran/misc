{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad
import Data.IORef
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.Random.TF
import System.Random.TF.Instances

type Coord = (Int, Int)

newtype Dir = Dir (Int, Int)

applyDir :: Dir -> Coord -> Coord
applyDir (Dir (dx, dy)) (x, y) = (x + dx, y + dy)

-- sends a specifc moving action at random interval, until done.
dirThread :: Dir -> IORef Bool -> TFGen -> (Dir -> IO ()) -> IO ()
dirThread d rDone g0 sendMove = do
  done <- readIORef rDone
  if done
    then pure ()
    else do
      -- whether to send a dir event.
      let (v, g1) = randomR (0 :: Int, 99) g0
      -- intentionally send a (0,0) movement so that
      -- we can see if no-diff are being filtered out.
      if (v < 50) then sendMove d else sendMove (Dir (0, 0))
      threadDelay 100_000
      dirThread d rDone g1 sendMove

main :: IO ()
main = do
  -- an indicator whether the simulation is considered finished
  rDone <- newIORef False
  (addDirHandler, sendDir :: Handler Dir) <- newAddHandler
  let printCurrentCoord :: Coord -> IO ()
      printCurrentCoord = print
      stopSimulation :: IO ()
      stopSimulation = atomicWriteIORef rDone True
      networkDesc :: MomentIO ()
      networkDesc = do
        eDir <- fromAddHandler addDirHandler
        eCoord <- accumE (0, 0) (fmap applyDir eDir)
        bCoord <- stepper (0, 0) eCoord
        curCoord <- valueB bCoord
        let diffA a b = if a == b then Nothing else Just a
            eDiff =
              -- this is not quite working for now, as curCoord stays at
              -- its initial value.
              filterJust $ fmap (diffA curCoord) eCoord
            eEnd = filterE id $ fmap (\(a, b) -> abs a + abs b > 5) eCoord
        reactimate $ fmap printCurrentCoord eDiff
        reactimate $ fmap (const stopSimulation) eEnd
        pure ()
  network <- compile networkDesc
  actuate network
  let dirs = fmap Dir [(-1, 0), (1, 0), (0, -1), (0, 1)]
  tasks <- forM dirs $ \dir -> do
    g <- newTFGen
    async $ dirThread dir rDone g sendDir
  mapM_ wait tasks
