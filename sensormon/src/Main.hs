{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import Prelude hiding (FilePath)

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Aeson
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Scientific
import System.Directory
import System.Exit
import System.Process
import Text.ParserCombinators.ReadP

import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M

{-
  the corresponding object looks like:

  - tempX_input
  - tempX_max
  - tempX_crit

  In which X is an integer. sensors seems to simply group them by X,
  so for parsing individual ones it is fine to just ignore it.
 -}
data TempInfo
  = TempInfo
  { tiInput :: Int
  , tiMax :: Maybe Int
  , tiCrit :: Maybe Int
  } deriving (Show)

parseTempField :: T.Text -> Maybe T.Text
parseTempField inp = case readP_to_S pTemp (T.unpack inp) of
    [(v, "")] -> Just v
    _ -> Nothing
  where
    pTemp :: ReadP T.Text
    pTemp =
      T.pack <$> (
        string "temp"
        *> munch1 isDigit
        *> char '_'
        *> munch1 (const True))

instance FromJSON TempInfo where
  parseJSON = withObject "TempInfo" $ \obj -> do
    let obj' :: Object
        obj' =
          HM.fromList
          . mapMaybe (\(k,v) -> (,v) <$> parseTempField k)
          $ HM.toList obj
        cov = round @Scientific
    TempInfo
      <$> fmap cov (obj' .: "input")
      <*> (fmap . fmap) cov (obj' .:? "max")
      <*> (fmap . fmap) cov (obj' .:? "crit")

{-
  This program parses output from `sensors` and print out useful info
  regarding temperature readings.

  Eventually this will end up being info source to some xmonad components.

 -}

{-
  naming here is a bit confusing, the following doc looks authoritative on this topic:

  https://www.kernel.org/doc/Documentation/hwmon/sysfs-interface
 -}

data Criticality = CNormal | CHigh | CCritical deriving Show
type TempDisplay = (Int, Criticality)

readFromSensors :: String -> IO (Maybe TempDisplay)
readFromSensors binPath = do
  let cp =
        (shell $ unwords [binPath, "-j", "-A"])
          { std_in = NoStream
          , std_out = CreatePipe
          , std_err = Inherit
          }
  (_, Just hOut, _, ph) <- createProcess cp
  ExitSuccess <- waitForProcess ph
  raw <- BSL.hGetContents hOut
  case eitherDecode' @(M.Map T.Text (M.Map T.Text (Maybe TempInfo))) raw of
    Left _e -> pure Nothing
    Right parsed -> do
      let tInfoList :: [TempInfo]
          tInfoList = foldMap (catMaybes . M.elems) parsed
          {-
            Let's only show the maximum temp - all of those might
            have a different definition for max, crit etc.
            But we do get the general idea of which part of the component stands out.
           -}
          ti = maximumBy (comparing tiInput) tInfoList
          inp = tiInput ti
          Just crit =
              shouldShowCrit
              <|> shouldShowHigh
              <|> Just CNormal
            where
              shouldShowCrit = do
                critBound <- tiCrit ti
                guard $ inp >= critBound
                pure CCritical
              shouldShowHigh = do
                highBound <- tiMax ti
                guard $ inp >= highBound
                pure CHigh
      pure $ Just (inp, crit)

main :: IO ()
main = do
  Just sensorsBinPath <- findExecutable "sensors"
  forever $ do
    putStrLn ""
    readFromSensors sensorsBinPath >>= print
    threadDelay $ 1000 * 1000
