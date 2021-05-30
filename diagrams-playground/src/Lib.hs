{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
  ( main
  )
where

-- import Diagrams.Backend.SVG.CmdLine

import Control.Monad
import Diagrams.Angle
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith example

-- cairoUnit :: Diagram Cairo
cairoUnit =
  (((reflectY cairoPen # snugB)
      <> (hcat
            [ cairoPen # rotate (tau / 4 @@ rad)
            , cairoPen # rotate ((- tau / 4) @@ rad)
            ]
            # centerX
            # snugT))
     # snugB
     <> cairoPen
     # snugT)
    # center

exampleFourCompo :: Diagram B
exampleFourCompo =
  (((cairoUnit # snugR <> ((cairoUnit # alignY 1) # snugL)) # snugL
      <> (cairoUnit # alignY 1) # snugR)
     # center)
    # snugB
    <> snugT cairoUnit

gen hf sz = do
  hInd <- [0 .. sz -1]
  (do
     wInd <- [- hInd .. hInd]
     pure $ p2 (wInd * 2 * hf, (-2) * hf * hInd))
    <> (do
          wInd <- [1 .. hInd + 1]
          [ p2 (- (2 * wInd -1) * hf, (-2) * hf * hInd - hf)
            , p2 ((2 * wInd -1) * hf, (-2) * hf * hInd - hf)
            ])

example :: Diagram B
example =
  atPoints
    (gen hf 8)
    (repeat cairoUnit)
  where
    hf :: Double
    hf = height (cairoUnit :: D V2 Double) / 2

-- cairoPen :: Diagram Cairo
cairoPen =
  polygon
    (with
       & polyType
         .~ PolySides
           [tau / 4 @@ rad, tau / 6 @@ rad, tau / 4 @@ rad, tau / 6 @@ rad]
           [1, 1, 1, 1])
