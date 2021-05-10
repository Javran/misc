{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}

module Unit_5
  ( main
  )
where

import Data.Bool
import qualified Data.Set as S
import Graphics.Image hiding (cols, rows)
import Graphics.Image.Interface
import System.Environment
import System.Exit

type SkipThenKeep = (Int, Int)

{-
  Collect points but taking advantage that we are collecting results from iterated functions:
  we can stop immediately once we've seen a number in the collected set - since that number
  has been collected before, all of its consequent numbers should also have been collected.
 -}
collectIterPoints :: SkipThenKeep -> [Double] -> S.Set Double
collectIterPoints (s, k) = collectIters S.empty . take k . drop s
  where
    collectIters :: S.Set Double -> [Double] -> S.Set Double
    collectIters acc [] = acc
    collectIters acc (x : xs) =
      if S.member x acc
        then acc
        else collectIters (S.insert x acc) xs

restrictToRange :: (Double, Double) -> S.Set Double -> S.Set Double
restrictToRange (xLo, xHi) xs =
  (if hasHi then S.insert xHi else id) $
    (if hasLo then S.insert xLo else id)
      xs''
  where
    (_, hasLo, xs') = S.splitMember xLo xs
    (xs'', hasHi, _) = S.splitMember xHi xs'

renderLine :: Int -> (Double, Double) -> S.Set Double -> [Bool]
renderLine cols xRange@(xLo, xHi) xs = fmap tr [0 .. cols -1]
  where
    xDist = xHi - xLo
    cols' = fromIntegral cols
    markedCols :: S.Set Int
    markedCols =
      S.map (\x -> round (cols' * (x - xLo) / xDist)) $
        restrictToRange xRange xs
    tr c = S.member c markedCols

-- initial value and how many to skip then keep.
type SampleMethod = (Double, SkipThenKeep)

type Dims = (Int, Int) -- (width, height)

data Plot = Plot
  { plDims :: Dims
  , plFunc :: Double -> Double -> Double
  , plRangeR :: (Double, Double)
  , plRangeX :: (Double, Double)
  , plSampleMethod :: SampleMethod
  }

renderPlot :: Plot -> [[Bool]]
renderPlot pl =
  fmap
    (\r ->
       let t = collectIterPoints stk $ iterate f seed
           f = plFunc r
        in renderLine cols plRangeX t)
    (take rows [rLo, rLo + step ..])
  where
    Plot
      { plDims = (rows, cols)
      , plFunc
      , plRangeX
      , plRangeR = (rLo, rHi)
      , plSampleMethod = (seed, stk)
      } = pl
    step = (rHi - rLo) / fromIntegral (rows -1)

plots :: [(String, Plot)]
plots =
  [ ( "a1"
    , Plot
        { plDims
        , plFunc = \r x -> r * x * (1 - x)
        , plRangeX = (0.7765042979942693, 0.9140401146131805)
        , plRangeR = (3.536511882631319, 3.593323318496883)
        , plSampleMethod
        }
    )
  , ( "a2"
    , Plot
        { plDims
        , plFunc = \r x -> r * x * x * (1 - x)
        , plRangeX = (0.7917471809417964, 0.8925554245570697)
        , plRangeR = (5.847883456366569, 5.991903306661956)
        , plSampleMethod
        }
    )
  , ( "a3"
    , Plot
        { plDims
        , plFunc = \r x -> r * sin (pi * x / 2)
        , plRangeX =
            takeL 0.6
              . takeL 0.3
              . takeR 0.5
              $ (0, 3)
        , plRangeR =
            takeR 0.5
              . takeR 0.5
              . takeL 0.5
              . takeL 0.3
              . takeR 0.7
              . takeR 0.5
              . takeL 0.5
              $ (0, 5)
        , plSampleMethod
        }
    )
  ]
  where
    takeL p (lo, hi) = (lo, lo + d * p)
      where
        d = hi - lo
    takeR p (lo, hi) = (hi - d * p, hi)
      where
        d = hi - lo

    plDims = (2000, 1200)
    plSampleMethod = (0.5, (1000, 2000))

main :: IO ()
main =
  getArgs >>= \case
    [whichPlot, target]
      | Just pl <- lookup whichPlot plots -> do
        let rendered = renderPlot pl
            y = PixelRGB 0 0 255
            n = PixelRGB 255 255 255
            imgParallel =
              rotate270 $
                fromListsR RPS $
                  (fmap . fmap) (bool n y) rendered
            img :: Image VS RGB Word8
            img = toManifest imgParallel
        writeImageExact PNG [] target img
    _ -> do
      putStrLn "usage: <prog> <which> <target image>"
      putStrLn $ "which: " <> show (fmap fst plots)
      exitFailure
