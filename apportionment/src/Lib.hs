{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import Control.Monad
import Control.Monad.State.Strict
import Data.List
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Population

type M = StateT (M.Map T.Text Int) IO

{-
  TODO: just want to follow US congressional apportionment process for fun.

  https://en.wikipedia.org/wiki/United_States_congressional_apportionment#Apportionment_methods
 -}
main :: IO ()
main = do
  _ <- runStateT (replicateM (385+5) assignNextSeat) initSeats
  pure ()

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
    T.putStrLn $ pKey <> ", " <> T.pack (show pVal) <> ", " <> T.pack (show (1 + seats M.! pKey))
  modify (M.adjust succ pKey)
