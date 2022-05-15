{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  )
where

import qualified CapacityLevel as CL
import DBus
import DBus.Client
import Data.String
import Data.Word
import qualified PowerStatus as PS
import System.Directory
import System.FilePath.Posix
import Text.ParserCombinators.ReadP
import Control.Monad

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
      pure $
        Just $ case readP_to_S (parser <* skipSpaces <* eof) raw of
          [(v, "")] -> Right v
          _ -> Left raw
    else pure Nothing

type InhibitorInfo = (String, String, String, String, Word32, Word32)

displayInhibitorInfo :: InhibitorInfo -> IO ()
displayInhibitorInfo (what, who, why, mode, uid, pid) = do
  putStrLn $ "What: " <> what
  putStrLn $ "Who: " <> who
  putStrLn $ "Why: " <> why
  putStrLn $ "Mode: " <> mode
  putStrLn $ "UID: " <> show uid
  putStrLn $ "PID: " <> show pid


examineSys :: Client -> IO ()
examineSys sys = do
  reply <-
    call_
      sys
      (methodCall
         "/org/freedesktop/login1"
         "org.freedesktop.login1.Manager"
         "ListInhibitors")
        { methodCallDestination = Just "org.freedesktop.login1"
        }
  let [r] = methodReturnBody reply
      xs :: [InhibitorInfo]
      Just xs = fromVariant r
  forM_ xs $ \x -> do
    displayInhibitorInfo x
    putStrLn ""
  pure ()

{-
  For ListInhibitors: what, who, why, mode, uid (user ID), and pid

  Ref: https://www.freedesktop.org/software/systemd/man/org.freedesktop.login1.html
 -}
main :: IO ()
main = do
  readAndParse "capacity_level" CL.capacityLevelP >>= print
  readAndParse "status" PS.statusP >>= print

  putStrLn "# DBus"
  -- sys <- connectSystem
  -- examineSys sys
  sess <- connectSession
  reply <-
    call_
      sess
      (methodCall
         "/org/freedesktop/PowerManagement/Inhibit"
         "org.freedesktop.PowerManagement.Inhibit"
         "GetInhibitors")
        { methodCallDestination = Just "org.freedesktop.PowerManagement"
        }
  let [r] = methodReturnBody reply
      xs :: [String]
      Just xs = fromVariant r
  print xs
