{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LotkaVolterra
  ( main
  )
where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

locs :: [(Double, Double)]
locs = [(r, f) | r <- [0.001, 0.2 .. 10], f <- [0.001, 0.2 .. 10]]

arrows :: [Diagram B]
arrows = map arrowAtPoint locs
  where
    arrowAtPoint (x, y) = arrowAt' opts (p2 (x, y)) (sL *^ vf)
      where
        vf = let v = r2 $ rateOfChange (x, y) in if v == 0 then 1 else v
        -- m = norm $ vectorField (x, y)
        sL = 0.05
        opts =
          with & arrowHead .~ spike
            & headLength .~ normalized 0.01
            & shaftStyle %~ lwN 0.004

rateOfChange :: (Double, Double) -> (Double, Double)
rateOfChange (r, f) =
  -- TODO: example from open course, to be generalized later
  (r - r * f / 4, 0.2 * r * f - 0.6 * f)

example :: Diagram B
example = position (zip points arrows) # alignTL
  where
    points = map p2 locs

main :: IO ()
main = mainWith example
