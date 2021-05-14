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
  let locRadius = 0.05
      rMid = 3
      fMid = 4
      ptCount = 50 :: Int
      step = (locRadius + locRadius) / fromIntegral (ptCount - 1)
      rStart = rMid - locRadius
      fStart = fMid - locRadius
  r <- [rStart, rStart + step .. rMid + locRadius]
  f <- [fStart, fStart + step .. fMid + locRadius]
  pure (r, f)

arrows :: [Diagram B]
arrows = map arrowAtPoint locs
  where
    longest = maximum $ fmap (norm . r2 . rateOfChange) locs
    arrowAtPoint (x, y) = if vf == 0 then mempty else arrowAt' opts (p2 (x, y)) (sL *^ vf)
      where
        vf = r2 $ rateOfChange (x, y)
        m = norm vf
        sL =
          -- note that this constant has to be adjusted per `step` change in `locs`
          -- we should probably float that definition so both can see and adjust automatically.
          0.005 / longest
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
