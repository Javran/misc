{-# LANGUAGE
    OverloadedStrings
  , TupleSections
  , TypeApplications
  #-}
module Main
  ( main
  ) where

import Prelude hiding (FilePath)

import Data.Aeson
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Ord
import Data.Scientific
import Data.Text.Encoding (encodeUtf8)
import Filesystem.Path.CurrentOS hiding (null)
import System.Exit
import Text.ParserCombinators.ReadP
import Turtle.Prelude

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
toText' :: FilePath -> T.Text
toText' = either id id . toText

{-
  naming here is a bit confusing, the following doc looks authoritative on this topic:

  https://www.kernel.org/doc/Documentation/hwmon/sysfs-interface
 -}

type TempInfoTable = M.Map T.Text [TempInfo] -- this list is guaranteed to be non-empty

renderInfo :: T.Text -> TempInfoTable -> T.Text
renderInfo k tbl = case tbl M.!? k of
  Nothing -> "Unknown"
  Just ts ->
    let ti = maximumBy (comparing tiInput) ts
        criticality = "Normal" -- TODO
    in T.pack $ criticality <> ", " <> show (tiInput ti)

displayInfo :: T.Text -> TempInfoTable -> IO ()
displayInfo k tbl = do
  putStrLn (T.unpack k)
  putStrLn $ "  " <> T.unpack (renderInfo k tbl)

main :: IO ()
main = do
  Just sensorsBinPath <- which "sensors"
  (ExitSuccess, rawOut) <- procStrict (toText' sensorsBinPath) ["-j", "-A"] ""
  case
      eitherDecode' @(M.Map T.Text (M.Map T.Text (Maybe TempInfo)))
      . BSL.fromStrict
      . encodeUtf8
      $ rawOut of
    Left e -> print e
    Right parsed -> do
      let tbl :: TempInfoTable
          tbl =
            -- non-empty list only, with those that can be parsed successfully.
            M.filter (not . null)
            . M.map (catMaybes . M.elems)
            $ parsed
      mapM_
        (\k -> displayInfo k tbl)
        ["acpitz-acpi-0", "coretemp-isa-0000", "intended-missing"]
