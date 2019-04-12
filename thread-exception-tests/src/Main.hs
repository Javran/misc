{-# LANGUAGE LambdaCase #-}
module Main
  ( main
  ) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception
import Control.Monad
import System.IO
import System.Random

{-

  for some not-yet-explained reason, restarting
  a looping thread outside of errHandler doesn't seem
  to be reliably working when it comes to interaction with
  twitter-conduit (could be http-client),
  the following example simulates a frequently crashing thread
  with restart mechanism intentionally placed outside of errHandler
  to see if we can reproduce the situation.

  for now it all seems to work just fine:
  since t1 takes literally forever to run,
  waitEither should block indefinitely,
  we'll quit whenever t0 crashes, which isn't supposed to happen
  due to the fact that the "catch'em all" protection is set up.

-}

errHandler :: SomeException -> IO (Maybe a)
errHandler _e =
    pure Nothing

threadLoop :: StdGen -> IO ()
threadLoop s = catch (Just <$> step s) errHandler >>= \case
    Nothing -> threadLoop s
    Just s' -> threadLoop s'
  where
    step :: StdGen -> IO StdGen
    step sg = do
      let (v, sg') = next sg
      when (v `mod` 16 < 5) $ error "crash"
      threadDelay $ 1000 * 20
      pure sg'

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    sg <- newStdGen
    t0 <- async (threadLoop sg)
    t1 <- async (forever (threadDelay (1000 * 1000 * 10)) >> pure ())
    e <- waitEither t0 t1
    print e
