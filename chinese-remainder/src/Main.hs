{-
  Simulate solving Chinese remainder problem the naive way.
 -}
module Main
  ( main
  )
where

import Data.IORef
import Data.List
import Reactive.Banana
import Reactive.Banana.Frameworks

type State = Int

data CheckResult
  = Unsat Int -- condition is not satisfieid, requesting a increment
  | Sat -- condition is satisfied
  deriving (Eq)

remCheck :: Int -> Int -> Int -> CheckResult
remCheck n targetR v = case compare r targetR of
  EQ -> Sat
  LT -> Unsat (targetR - r)
  GT -> Unsat (n + targetR - r)
  where
    r = rem v n

checks :: [Int -> CheckResult]
checks =
  [ remCheck 3 2
  , remCheck 5 3
  , remCheck 7 2
  ]

mergeCheckResult :: CheckResult -> CheckResult -> CheckResult
mergeCheckResult l r = case (l, r) of
  (Sat, _) -> r
  (_, Sat) -> l
  (Unsat u, Unsat v) -> Unsat (max u v)

checkAll :: Int -> CheckResult
checkAll =
  foldr (\f g v -> mergeCheckResult (f v) (g v)) (const Sat) checks

main :: IO ()
main = do
  print
    (unfoldr
       (\v -> case checkAll v of
          Sat -> Nothing
          Unsat d -> Just $ let v' = v + d in (v', v'))
       0)
