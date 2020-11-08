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

type M = StateT (M.Map T.Text Int) IO

{-
  TODO: just want to follow US congressional apportionment process for fun.

  https://en.wikipedia.org/wiki/United_States_congressional_apportionment#Apportionment_methods
 -}
main :: IO ()
main = do
  _ <- runStateT (replicateM 20 assignNextSeat) initSeats
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

-- source: https://en.wikipedia.org/wiki/2010_United_States_Census
-- TODO: https://www.youtube.com/watch?v=6JN4RI7nkes might explain priority value difference,
-- as it appears to be a different population dataset that we are using.
populations :: M.Map T.Text Int
populations =
  M.fromList
    [ ("California", 37253956)
    , ("Texas", 25145561)
    , ("New York", 19378102)
    , ("Florida", 18801310)
    , ("Illinois", 12830632)
    , ("Pennsylvania", 12702379)
    , ("Ohio", 11536504)
    , ("Michigan", 9883640)
    , ("Georgia", 9687653)
    , ("North Carolina", 9535483)
    , ("New Jersey", 8791894)
    , ("Virginia", 8001024)
    , ("Washington", 6724540)
    , ("Massachusetts", 6547629)
    , ("Indiana", 6483802)
    , ("Arizona", 6392017)
    , ("Tennessee", 6346105)
    , ("Missouri", 5988927)
    , ("Maryland", 5773552)
    , ("Wisconsin", 5686986)
    , ("Minnesota", 5303925)
    , ("Colorado", 5029196)
    , ("Alabama", 4779736)
    , ("South Carolina", 4625364)
    , ("Louisiana", 4533372)
    , ("Kentucky", 4339367)
    , ("Oregon", 3831074)
    , ("Oklahoma", 3751351)
    , ("Connecticut", 3574097)
    , ("Iowa", 3046355)
    , ("Mississippi", 2967297)
    , ("Arkansas", 2915918)
    , ("Kansas", 2853118)
    , ("Utah", 2763885)
    , ("Nevada", 2700551)
    , ("New Mexico", 2059179)
    , ("West Virginia", 1852994)
    , ("Nebraska", 1826341)
    , ("Idaho", 1567582)
    , ("Hawaii", 1360301)
    , ("Maine", 1328361)
    , ("New Hampshire", 1316470)
    , ("Rhode Island", 1052567)
    , ("Montana", 989415)
    , ("Delaware", 897934)
    , ("South Dakota", 814180)
    , ("Alaska", 710231)
    , ("North Dakota", 672591)
    , ("Vermont", 625741)
    , ("Wyoming", 563626)
    ]
