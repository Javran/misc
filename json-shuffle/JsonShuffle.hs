module JsonShuffle where

import qualified Data.ByteString.Lazy as LBS
import System.Environment
import Data.Aeson

transform :: LBS.ByteString -> (LBS.ByteString, Bool)
transform b = (b1, b1 == b)
  where
    d = decode' b :: Maybe Value
    b1 = encode d

tryFix :: Int -> LBS.ByteString -> IO LBS.ByteString
tryFix 0 b = return b
tryFix t b = if flag
    then do
      putStrLn "Fixpoint reached."
      return b1
    else do
      putStrLn "Not reaching fixpoint, next iteration."
      tryFix (pred t) b1
  where
    (b1, flag) = transform b

-- making use of Haskell's purity.
-- by decoding and encoding again, all information of permutation
-- should be destroyed, leaving a fixpoint data d such that:
-- d == (encode . decode) d

main :: IO ()
main = do
    [fn] <- getArgs
    content <- LBS.readFile fn
    result <- tryFix 10 content
    LBS.writeFile (fn ++ "_norm") result
    return ()
