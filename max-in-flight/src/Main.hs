module Main
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.QSem
import Control.Monad

{-
  For exploring running concurrent tasks simultaneously
  while limiting the max number of tasks.
 -}

doStuff :: QSem -> a -> IO a
doStuff qs v = do
  waitQSem qs
  -- be careful here: if any exception happens here,
  -- the lock won't be properly released.
  threadDelay (1000 * 1000 * 2)
  signalQSem qs
  pure v

main :: IO ()
main = do
  qs <- newQSem 4
  as <- mapM (async . doStuff qs) [0..19]
  mapM_ (wait >=> print) as
