module Unit_3
  ( main
  )
where

import Control.Monad

logisticIter r = iterate next
  where
    next x = r * x * (1 - x)

main :: IO ()
main = forM_ (zip [0 .. 20] (logisticIter 3.7 0.9)) $ \(i, v) ->
  putStrLn $ show i <> ": " <> show v
