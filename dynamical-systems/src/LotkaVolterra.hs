{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module LotkaVolterra
  ( main
  )
where

import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

locs :: [(Double, Double)]
locs = do
  let radius = 0.5
      rMid = 3
      fMid = 4
      ptCount = 20 :: Int
      step = (radius + radius) / fromIntegral (ptCount - 1)
      rStart = rMid - radius
      fStart = fMid - radius
  r <- [rStart, rStart + 0.01 .. rMid + radius]
  f <- [fStart, fStart + 0.01 .. fMid + radius]
  pure (r, f)

-- [(r, f) | r <- [0, 0.2 .. 8], f <- [0, 0.2 .. 8]]

arrows :: [Diagram B]
arrows = map arrowAtPoint locs
  where
    longest = maximum $ fmap (norm . r2 . rateOfChange) locs
    arrowAtPoint (x, y) = if vf == 0 then mempty else arrowAt' opts (p2 (x, y)) (sL *^ vf)
      where
        vf = r2 $ rateOfChange (x, y)
        m = norm vf
        sL = 0.5 / longest
        opts =
          with & arrowHead .~ spike
            & headLength .~ normalized (0.015 * m / longest)
            & shaftStyle %~ lwN (0.004 * m / longest)

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
