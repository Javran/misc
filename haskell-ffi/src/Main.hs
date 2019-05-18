{-# LANGUAGE ForeignFunctionInterface, ScopedTypeVariables #-}
module Main
  ( main
  ) where

import Foreign.Ptr
import Foreign.Marshal.Alloc

data NeumaierObj

foreign import ccall "neumaier_sum_init"
  neumaierSumInit :: Ptr NeumaierObj -> IO ()

foreign import ccall "neumaier_sum_add"
  neumaierSumAdd :: Ptr NeumaierObj -> Double -> IO ()

foreign import ccall "neumaier_sum_done"
  neumaierSumDone :: Ptr NeumaierObj -> IO Double

main :: IO ()
main = do
  let xs = [1,1.1..20]
  result <- allocaBytes 16 $ \(p :: Ptr NeumaierObj) -> do
    neumaierSumInit p
    mapM_ (neumaierSumAdd p) xs
    neumaierSumDone p
  print (result :: Double, sum xs)
