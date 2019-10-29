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
  TODO: the current implementaion does not work as part of SysInfoBar for my xmonad config.
  I suspect "turtle" might have done some weird stuff that messed up threading.
  Let's try to write this directly using CreateProcess without using "turtle" package.
 -}

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

type TempInfoTable = M.Map T.Text [TempInfo] -- this list is guaranteed to be non-empty
data Criticality = CNormal | CHigh | CCritical deriving Show
type TempDisplay = (Int, Criticality)

readFromSensors :: String -> IO (Maybe TempDisplay, Maybe TempDisplay)
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
    Left _e -> pure (Nothing, Nothing)
    Right parsed -> do
      let tbl :: TempInfoTable
          tbl =
            -- non-empty list only, with those that can be parsed successfully.
            M.filter (not . null)
            . M.map (catMaybes . M.elems)
            $ parsed
          toDisplay :: T.Text -> Maybe TempDisplay
          toDisplay prop = do
            vs <- tbl M.!? prop
            let ti = maximumBy (comparing tiInput) vs
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
            pure (inp, crit)
      pure (toDisplay "coretemp-isa-0000", toDisplay "acpitz-acpi-0")

main :: IO ()
main = do
  Just sensorsBinPath <- findExecutable "sensors"
  forever $ do
    putStrLn ""
    readFromSensors sensorsBinPath >>= print
    threadDelay $ 1000 * 1000
