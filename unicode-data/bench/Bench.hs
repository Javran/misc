module Bench
  ( main
  )
where

import Control.DeepSeq
import Criterion.Main
import Lib (benchmarker)
import qualified UnicodeV13 as U13
import qualified UnicodeV13Packed as U13P
import qualified Data.Char as BaseChar

main :: IO ()
main =
  defaultMain
    [ bench "U13" $ nf benchmarker U13.generalCategory
    , bench "U13P" $ nf benchmarker U13P.generalCategory
    , bench "base" $ nf benchmarker BaseChar.generalCategory
    ]
