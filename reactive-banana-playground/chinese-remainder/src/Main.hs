{-
  Simulate solving Chinese remainder problem the naive way.

  We'll start with s=0 state, together with 3 checks to verify that:

  - s `mod` 3 == 2
  - s `mod` 5 == 3
  - s `mod` 7 == 2

  Checker replies `Sat` if it is satisfied,
  otherwise `Unsat v`, where `v` is a suggested increment
  to the state so that `n+v` satisfies this checker.

  Every round the network will look at result from 3 checkers:
  - if all of them are `Sat`, we are done here.
  - otherwise merge then by constructing `Unsat v` where v is the max of
    all `Unsat`s.

  This system does not always terminate given different set of checkers,
  but in this well-known case, it does.

 -}
{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Control.Concurrent
import Control.Concurrent.Async
import Data.Function
import Data.List
import Reactive.Banana
import Reactive.Banana.Frameworks

data CheckResult
  = Unsat Int -- condition is not satisfieid, requesting a increment
  | Sat -- condition is satisfied
  deriving (Eq, Show)

remCheck :: Int -> Int -> Int -> CheckResult
remCheck n targetR v = case compare r targetR of
  EQ -> Sat
  LT -> Unsat (targetR - r)
  GT -> Unsat (n + targetR - r)
  where
    r = rem v n

checks :: [Int -> CheckResult]
checks = [ remCheck 3 2 , remCheck 5 3 , remCheck 7 2]

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
  putStrLn $ "Expecting: "
  print
    (unfoldr
       (\v -> case checkAll v of
          Sat -> Nothing
          Unsat d -> Just $ let v' = v + d in (v', v'))
       0)
  ch <- newChan
  (addHandler :: AddHandler Int, requestIncrement :: Int -> IO ()) <- newAddHandler
  h <- async $
    {-
      this is the solver thread, it looks at increment requests from the network,
      and feed requested increment back into the network.
     -}
    fix $ \next -> do
      r <- readChan ch
      print r
      case r of
        Sat -> pure ()
        Unsat x -> requestIncrement x >> next
  let resultCallback :: CheckResult -> IO ()
      resultCallback = writeChan ch
      networkDesc :: MomentIO ()
      networkDesc = do
        eIncr <- fromAddHandler addHandler
        -- accumulated state
        eSum <- accumE (0 :: Int) $ fmap (+) eIncr
        let eChecks :: Event [CheckResult]
            -- current state of all checks
            eChecks = fmap (\v -> fmap ($ v) checks) eSum
            -- fold result, to see what increment should we try next
            bCheckResult = fmap (foldr mergeCheckResult Sat) eChecks
        -- trigger IO to send request to solver.
        reactimate $ fmap resultCallback bCheckResult
        reactimate $ fmap (\v -> putStrLn $ "Current sum: " <> show v) eSum
        reactimate $ fmap (\chks -> putStrLn $ "Checkers: " <> show chks) eChecks
  network <- compile networkDesc
  {-
    this request "activates" the network,
    its purpose is to trigger an initial event
    to start driving the network.

    Note that no multi-threading stuff is needed
    as it turns out "actuate" is not blocking.
   -}
  actuate network
  requestIncrement 0
  wait h
