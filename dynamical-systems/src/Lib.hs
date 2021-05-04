module Lib
  ( main
  )
where

import Control.Monad

eulerMethod fRateOfChange dt seed = zip [0, dt ..] (iterate next seed)
  where
    next st = st + rateOfChange * dt
      where
        rateOfChange = fRateOfChange st

exactSol seed t = 20 - (20 - seed) * exp (-0.2 * t)

main :: IO ()
main = do
  let seed = 100
      step = 1
      progression = eulerMethod (\t -> 0.2 * (20 - t)) step seed
  forM_ (takeWhile ((<= 60) . fst) progression) $ \(t, eVal) -> do
    putStrLn $
      "t="
        <> show t
        <> ", val="
        <> show eVal
        <> ", truth="
        <> show (exactSol seed t)
