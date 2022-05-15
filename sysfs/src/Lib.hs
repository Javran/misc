{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified CapacityLevel as CL
import Data.String
import qualified PowerStatus as PS
import System.Directory
import Text.ParserCombinators.ReadP
import System.FilePath.Posix

{-
  For now just some power related stuff.

  Ref: https://www.kernel.org/doc/Documentation/ABI/testing/sysfs-class-power

 -}

defSupply :: IsString s => s
defSupply = "/sys/class/power_supply/hidpp_battery_0"

readAndParse :: String -> ReadP a -> IO (Maybe (Either String a))
readAndParse what parser = do
  b <- doesDirectoryExist defSupply
  if b
    then do
      raw <- readFile $ defSupply </> what
      pure $ Just $ case readP_to_S (parser <* skipSpaces <* eof) raw of
        [(v, "")] -> Right v
        _ -> Left raw
    else pure Nothing

main :: IO ()
main = do
  readAndParse "capacity_level" CL.capacityLevelP >>= print
  readAndParse "status" PS.statusP >>= print
  pure ()
