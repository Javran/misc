{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LotkaVolterra
  ( main
  )
where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

locs :: [(Double, Double)]
locs = [(r, f) | r <- [0, 0.5 .. 20], f <- [0, 0.5 .. 20]]

points = map p2 locs

vectorField :: (Double, Double) -> V2 Double
vectorField (r, f) = let (dr, df) = rateOfChange (r, f) in r2 (dr, df)

arrows = map arrowAtPoint locs
  where
    arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf) # alignTL
      where
        vf = 0.1 + vectorField (x, y)
        m = norm $ vectorField (x, y)
        hs = 0.02
        sW = 0.004
        sL = 0.05
        opts =
          with & arrowHead .~ spike
            & headLength .~ normalized hs
            & shaftStyle %~ lwN sW

rateOfChange :: (Double, Double) -> (Double, Double)
rateOfChange (r, f) =
  -- TODO: example from open course, to be generalized later
  (r - r * f / 4, 0.2 * r * f - 0.6 * f)

example :: Diagram B
example =
  field # translateY 0.05
    <> (square 20 # lw none # alignBL)
  where
    field = position $ zip points arrows

main :: IO ()
main = mainWith example
