{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module KcNavyAlbum.DefaultDigest where

import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Network.HTTP.Client
import System.Exit

subCmdMain :: Manager -> String -> IO ()
subCmdMain mgr _cmdHelpPrefix = do
  req <- parseRequest "https://raw.githubusercontent.com/kcwiki/kancolle-data/master/api/api_start2.json"
  resp <- httpLbs req mgr
  case Aeson.eitherDecode @Aeson.Object (responseBody resp) of
    Left msg -> die msg
    Right r -> print (HM.keys r)
  pure ()
