{-# LANGUAGE TypeApplications #-}
module Main
  ( main
  ) where

import qualified Data.ByteString as BS
import Data.Bits
import System.IO
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Par.Class
import Control.Monad.Par.IO

-- some random task to perform. in this case
-- we fetch some random bytes from /dev/urandom.
performTask :: a -> IO (a, Integer)
performTask n = do
  r <- withBinaryFile "/dev/urandom" ReadMode $
    \h -> BS.hGet h 4
  let [a,b,c,d] = fromIntegral @_ @Integer <$> BS.unpack r
  pure (n, a .|. shiftL b 8 .|. shiftL c 16 .|. shiftL d 24)

tasks :: [] Char
tasks = ['0' .. '9'] <> ['A' .. 'Z'] <> ['a' .. 'z']

work :: ParIO [(Char, Integer)]
work = do
  start <- new
  end <- new
  taskVars <- zipWithM const (repeat new) tasks
  forM_ (zip tasks taskVars) $ \(t,tv) -> fork $ do
    _ <- get start
    u <- liftIO (performTask t)
    put tv u
  fork $ do
    ys <- mapM get taskVars
    put end ys
  fork $ put start ()
  get end

main :: IO ()
main = runParIO work >>= print
