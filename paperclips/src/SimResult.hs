module SimResult where

import qualified Data.Map.Strict as M

import Types

type SimResult = (Int, [Score])

type SimResults = M.Map Payoff SimResult
