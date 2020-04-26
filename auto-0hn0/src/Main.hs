{-# LANGUAGE
    OverloadedStrings
  , ScopedTypeVariables
  , TupleSections
  #-}
module Main
  ( main
  ) where

import Control.Concurrent
import Control.Monad
import Data.Either
import Data.Function
import Data.List
import Data.Maybe
import Data.UUID.V1
import MatchingAgent.Server
import System.Console.Terminfo
import System.Directory
import System.Environment
import System.Exit
import System.Process
import System.Random.Shuffle

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Graphics.Image as HIP
import qualified Graphics.Image.IO as HIP
import qualified Graphics.Image.Processing.Binary as HIP

import Solver

type Pixel = HIP.Pixel HIP.RGBA HIP.Word8
type Image = HIP.Image HIP.VS HIP.RGBA HIP.Word8

screenCapture :: IO Image
screenCapture = do
  let cp =
        (proc "/usr/bin/adb" ["exec-out", "screencap", "-p"])
          { std_out = CreatePipe
          }
  (_, Just hOut, _, ph) <- createProcess cp
  imgRaw <- BS.hGetContents hOut
  ExitSuccess <- waitForProcess ph
  let Right img = HIP.decode HIP.PNG imgRaw
  pure img

coords :: [[(Int, Int)]]
coords = [ [ (r',c') | c' <- [41,152,263,374,486,598,710,822,934]] | r <- [0..8], let r' = 413+111*r]

collectSample :: IO ()
collectSample = do
  imgFull <- screenCapture
  let samples :: [((Int, Int), Image)]
      samples =
        concat
        . (fmap . fmap) (\coord@(r,c) -> (coord, HIP.crop (r+30, c+37) (39,28) imgFull))
        $ coords
  forM_ samples $ \((r,c), img) ->
    let fName = "samples/sample_" <> show r <> "_" <> show c <> ".png"
    in HIP.writeImageExact HIP.PNG [] fName img
  pure ()

getSampleName :: String -> Maybe (String, String)
getSampleName fName = do
  let tag = takeWhile (/= '_') fName
  guard $ tag /= "sample"
  guard $ ".png" `isSuffixOf` fName
  pure (fName, tag)

type Samples = M.Map String [Image]
type RevSamples = [(Image, String)]

loadSamples :: IO Samples
loadSamples = do
  fs <- mapMaybe getSampleName <$> listDirectory "samples"
  pairs <- forM fs $ \(fName, tag) -> do
    Right img <- HIP.readImageExact HIP.PNG ("samples/" <> fName)
    pure (tag, [img])
  pure $ M.fromListWith (<>) pairs

recognizeOrRecord :: RevSamples -> Image -> IO (Either Image String)
recognizeOrRecord rs img = do
  let threshold = 40
      matched = filter (\(patImg, _tag) -> HIP.eqTol threshold patImg img) rs
  case matched of
    (_, tag):_ -> pure (Right tag)
    [] -> pure (Left img)

captureSamples :: IO [[Image]]
captureSamples = do
  imgFull <- screenCapture
  pure $ (fmap . fmap) (\(r,c) -> HIP.crop (r+30, c+37) (39,28) imgFull) coords

{-
  Recognize cells and record sample images for those that we cannot recognize.
 -}
_analysisSamples :: IO ()
_analysisSamples = do
  samples <- loadSamples
  let sampleList = M.toList samples
  let thres = 200
  forM_ sampleList $ \(k, imgs) -> do
    let l = length imgs
    putStrLn $ "Tag: " <> k
    forM_ [0..l-1] $ \i -> do
      putStrLn [ if HIP.eqTol thres (imgs!!i) (imgs!!j) then 'T' else ' ' | j <- [0..l-1]]
  let allSamples = concatMap snd sampleList
  let l = length allSamples
  forM_ [0..l-1] $ \i -> do
    putStrLn [ if HIP.eqTol thres (allSamples!!i) (allSamples!!j) then 'T' else ' ' | j <- [0..l-1]]

findImageTag :: ServerHandle -> Image -> IO (T.Text, Float)
findImageTag h img = do
  let encoded :: BS.ByteString
      encoded = BSL.toStrict $ HIP.encode HIP.PNG [] img
  findTag h encoded

main :: IO ()
main = do
  term <- setupTermFromEnv
  serverConfig <-
    ServerConfig
      <$> getEnv "MA_SERVER_BIN_PATH"
      <*> (read <$> getEnv "MA_SERVER_PORT")
      <*> getEnv "MA_SERVER_PATTERN_BASE"
  withServer serverConfig $ \h -> do
    sps <- captureSamples
    matchResults :: [[(T.Text, Float)]] <- (mapM . mapM) (findImageTag h) sps
    let tr :: T.Text -> String
        tr "grey" = "?"
        tr "red" = "r"
        tr xs = T.unpack xs
        isGoodMatch = (> 0.999). snd
    case partition isGoodMatch (concat matchResults) of
      (_, []) -> do
        let ls = (fmap . fmap) (\(r, _) -> tr r) matchResults
            input = unwords <$> ls
        appendFile "puzzles.txt" (unlines $ input <> ["===="])
        solveAndShow term input
      {-
    (ls, _) -> do
      putStrLn $ "Failed to match " <> show (length ls) <> " items."
      when (length ls /= 9 * 9) $
        forM_ ls $ \img -> do
          sampleId <- fix $ \loop -> do
            v <- nextUUID
            maybe loop pure v
          let fName = "samples/sample_" <> show sampleId <> ".png"
          HIP.writeImageExact HIP.PNG [] fName img
   -}
  pure ()
