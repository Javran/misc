{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Lib
  ( main
  )
where

import Diagrams.Angle
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith (cairoUnit :: Diagram Cairo)

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

-- cairoPen :: Diagram Cairo
cairoPen =
  polygon
    (with
       & polyType
         .~ PolySides
           [tau / 4 @@ rad, tau / 6 @@ rad, tau / 4 @@ rad, tau / 6 @@ rad]
           [1, 1, 1, 1])
