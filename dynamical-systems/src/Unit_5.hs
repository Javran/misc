{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Unit_5
  ( main
  )
where

import Control.Monad
import Data.List.Extra (nubSort)
import qualified Data.Set as S
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude hiding (image, magnitude)
import System.Console.Terminfo
import Text.Printf

type SkipThenKeep = (Int, Int)

collectPoints :: SkipThenKeep -> [Double] -> [Double]
collectPoints (s, k) =
  nubSort
    . take k
    . drop s

renderLine :: Int -> (Double, Double) -> [Double] -> [Bool]
renderLine cols (xLo, xHi) xs = fmap tr [0 .. cols -1]
  where
    xDist = xHi - xLo
    cols' = fromIntegral cols
    markedCols :: S.Set Int
    markedCols =
      S.fromList
        . concatMap
          (\x ->
             [ round (cols' * (x - xLo) / xDist)
             | x >= xLo && x <= xHi
             ])
        $ xs
    tr c = S.member c markedCols

renderIteratedFunction
  :: (Double -> Double -> Double)
  -> (Int, Int)
  -> ((Double, Double), (Double, Double))
  -> [[Bool]]
renderIteratedFunction mkFunc (rows, cols) ranges =
  fmap
    (\r ->
       let t = collectPoints (100, 200) $ getSequence r 0.5
        in renderLine cols xRange t)
    (take rows [rLo, rLo + step ..])
  where
    getSequence :: Double -> Double -> [Double]
    getSequence r seed = iterate f seed
      where
        f = mkFunc r
    (xRange, (rLo, rHi)) = ranges
    step = (rHi - rLo) / fromIntegral (rows -1)

graph :: Diagram B
graph = (vcat . map hcat $ rendered) # bgFrame 3 white
  where
    rendered =
      (fmap . fmap) (\b -> square 1 # lw medium # fc black # opacity (if b then 1 else 0)) $
        renderIteratedFunction
          (\r x -> r * x * (1 - x))
          (600, 350)
          ( -- xRange
            (0.7765042979942693, 0.9140401146131805)
          , -- rRange
            (3.536511882631319, 3.593323318496883)
          )

main :: IO ()
main = mainWith graph

mainCli :: IO ()
mainCli = do
  term <- setupTermFromEnv
  let Just (rows, cols) = getCapability term ((,) <$> termLines <*> termColumns)
  printf "rows: %d, cols: %d\n" rows cols
  let wantRows = rows - 3
      rendered =
        renderIteratedFunction
          (\r x -> r * x * (1 - x))
          (wantRows, cols)
          ( -- xRange
            (0.7765042979942693, 0.9140401146131805)
          , -- rRange
            (3.536511882631319, 3.593323318496883)
          )

  forM_ rendered $ \rs -> do
    putStrLn $ (\b -> if b then '*' else ' ') <$> rs
  pure ()
