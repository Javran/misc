module Unit_5
  ( main
  )
where

import qualified Data.Set as S
import System.Console.Terminfo
import Text.Printf
import Control.Monad

type SkipThenKeep = (Int, Int)

collectPoints :: SkipThenKeep -> [Double] -> S.Set Double
collectPoints (s, k) =
  S.fromList
    . take k
    . drop s

getSequence :: Double -> Double -> [Double]
getSequence r seed = iterate f seed
  where
    f x = r * x * (1-x)

renderLine :: Int -> S.Set Double -> String
renderLine cols xs = fmap tr [0..cols-1]
  where
    cols' = fromIntegral cols
    markedCols :: S.Set Int
    markedCols = S.map (\v -> round (cols' * v)) xs
    tr c = if S.member c markedCols then '*' else ' '

main :: IO ()
main = do
  term <- setupTermFromEnv
  let Just (rows, cols) = getCapability term ((,) <$> termLines <*> termColumns)
  printf "rows: %d, cols: %d\n" rows cols
  let (rLo, rHi) = (2.88, 4.0)
      wantRows = rows - 3
      step = (rHi - rLo) / fromIntegral (wantRows -1)
      -- t = collectPoints (100,200) $ getSequence 3.7206 0.5
  forM_ (take wantRows [rLo,rLo+step..]) $ \r -> do
    let t = collectPoints (100,200) $ getSequence r 0.5
    putStrLn $ renderLine cols t
  pure ()
