{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.FilePath
import Web.Spock
import Web.Spock.Config
import System.Directory

import Control.Monad.Trans
import Data.Semigroup
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX

type MySession = ()
newtype MyAppState = AppState FilePath

main :: IO ()
main = do
    (timestamp :: Int) <- round <$> getPOSIXTime
    cwd <- getCurrentDirectory
    let instanceDir = cwd </> ("instance-" <> show timestamp)
    createDirectory instanceDir
    putStrLn $ "Instance dir is: " <> instanceDir
    spockCfg <- defaultSpockCfg () PCNoDatabase (AppState instanceDir)
    let sCfg = spockCfg {spc_maxRequestSize = Nothing}
    runSpock 19721 (spock sCfg app)

app :: SpockM () MySession MyAppState ()
app =
    do get root $
           text "Use post/<table-name>"
       post ("post" <//> var) $ \(tableName :: T.Text) -> do
           (AppState instanceDir) <- getState
           Just t <- param "time"
           Just raw <- param "raw"
           let outputPath = instanceDir </> fName
               fName = T.unpack tableName <> "-" <> T.unpack t <> ".raw"
           liftIO $ T.writeFile outputPath raw
           text "ok"
       post "post-table-ids" $ do
           (AppState instanceDir) <- getState
           Just raw <- param "raw"
           let outputPath = instanceDir </> fName
               fName = "table-ids.json"
           liftIO $ T.writeFile outputPath raw
           text "ok"
