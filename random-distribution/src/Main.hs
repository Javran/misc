{-# LANGUAGE ScopedTypeVariables #-}

module Main
  ( main
  )
where

import Control.Monad
import Control.Monad.Trans
import Data.Binary
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe
import Data.Random
import Data.Random.Source.MWC
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import System.Console.Terminfo

-- experiment to generate different random distributions
newtype SeedPack = SeedPack (V.Vector Word32)

spLen :: Int
spLen = 256

instance Binary SeedPack where
  put (SeedPack vs) = mapM_ put vs
  get = (SeedPack . V.fromListN spLen) <$> replicateM spLen get

drawFromURandom :: IO SeedPack
drawFromURandom = decode <$> BSL.readFile "/dev/urandom"

-- a Double is generated and multipled by `scale` to give range from 0 to scale-1
scaledExperiment :: Int -> Int -> IO (V.Vector Int)
scaledExperiment scale totalCount = do
  mv <- VM.replicate scale 0
  let roll :: RVarT IO ()
      roll = do
        (d :: Double) <- rvarT (Normal 0.5 0.2)
        when (d >= 0 && d < 1) $ do
          let result :: Int
              result = floor (fromIntegral scale * d)
          lift $ VM.modify mv succ result
  SeedPack seed <- drawFromURandom
  g <- initialize seed
  _ <- runRVarT (replicateM totalCount roll) g
  V.unsafeFreeze mv

main :: IO ()
main = do
  term <- setupTermFromEnv
  let rows = fromMaybe 20 $ fmap (subtract 4) $ getCapability term termLines
      cols = fromMaybe 80 $ getCapability term termColumns
  results <- scaledExperiment rows 100000
  let maxV = maximum results
      fills =
        V.map
          (\v ->
             round
               (fromIntegral (v * cols) / fromIntegral maxV :: Double)
             :: Int)
          results
  mapM_ (putStrLn . (\x -> replicate x '*')) fills
