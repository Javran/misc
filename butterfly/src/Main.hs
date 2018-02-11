{-# LANGUAGE NoMonomorphismRestriction, FlexibleContexts, TypeFamilies #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

rearrange :: [a] -> [a]
rearrange = uncurry (++) . divide
  where
    divide [] = ([],[])
    divide (x:y:as) = (x:xs', y:ys')
      where
        (xs',ys') = divide as
    divide _ = error "list length is odd"

main :: IO ()
main = mainWith (vsep 1 (replicate 4 (hrule 1 # lc blue)) :: Diagram B)
