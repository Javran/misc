{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Population
import Text.Printf

type M = StateT (M.Map T.Text Int) IO

{-
  Just want to follow US congressional apportionment process for fun, which is detailed in:
  https://en.wikipedia.org/wiki/United_States_congressional_apportionment#Apportionment_methods
 -}
main :: IO ()
main = do
  seats <- execStateT (replicateM 385 (assignNextSeat >> popPerSeatStat)) initSeats
  let sortedSeats = sortOn (negate . snd) $ M.toList seats
  putStrLn ""
  forM_ sortedSeats $ \(stName, seatCount) -> do
    let popPerSeat :: Double
        popPerSeat = fromIntegral (populations M.! stName) / fromIntegral seatCount
    printf "%s: %d, pop/seat = %.2f\n" (T.unpack stName) seatCount popPerSeat

initSeats :: M.Map T.Text Int
initSeats = M.map (const 1) populations

assignNextSeat :: M ()
assignNextSeat = do
  seats <- get
  let prios = M.mapWithKey prio seats
      prio k s = (fromIntegral pop :: Double) / sqrt (fromIntegral (s * (s + 1)) :: Double)
        where
          pop = populations M.! k
      sorted = sortOn (negate . snd) $ M.toList prios
      (pKey, pVal) = head sorted
  liftIO $
    printf "%s, %.2f, %d\n" (T.unpack pKey) pVal (1 + seats M.! pKey)
  modify (M.adjust succ pKey)

popPerSeatStat :: M ()
popPerSeatStat = do
  seats <- get
  let popSeats =
        M.toList $
          M.mapWithKey
            (\stName seatCount ->
               let pop :: Double
                   pop = fromIntegral $ populations M.! stName
                in pop / fromIntegral seatCount)
            seats
      (minSt, minV) = minimumBy (compare `on` snd) popSeats
      (maxSt, maxV) = maximumBy (compare `on` snd) popSeats
  liftIO $ do
    printf "pop/seat stat: diff: %.2f\n" (maxV - minV)
    printf "  min: %s, %.2f, max: %s, %.2f\n" (T.unpack minSt) minV (T.unpack maxSt) maxV
