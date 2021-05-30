{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
  ( main
  )
where

import Diagrams.Angle
import Diagrams.Backend.Cairo.CmdLine
-- import Diagrams.Backend.SVG.CmdLine
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

example :: Diagram B
example =
  atPoints
    [ p2 (0, 0)
    , p2 (hf, - hf)
    , p2 (- hf, - hf)
    , p2 (0, -2 * hf)
    ]
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
