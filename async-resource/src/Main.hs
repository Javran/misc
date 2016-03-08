module Main where

import Data.Conduit
import Control.Monad.IO.Class
import Control.Concurrent hiding (yield)
import Control.Monad.Random

infiniteProduce :: (Enum a, Ord a, MonadIO m) => a -> Source m a
infiniteProduce i = do
    yield i
    d <- liftIO $ getRandomR (1, 3 :: Int)
    liftIO $ threadDelay (1000 * 1000 * d)
    infiniteProduce (succ i)

debugSink :: Int -> Sink Int IO String
debugSink cnt | cnt <= 0 = pure "Done"
debugSink cnt = do
    result <- await
    liftIO $ print result
    debugSink (pred cnt)

main :: IO ()
main = do
    result <- newResumableSource (infiniteProduce (0 :: Int)) $$+- debugSink 4
    print result
    pure ()
