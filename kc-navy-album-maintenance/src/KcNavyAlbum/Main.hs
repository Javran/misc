{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module KcNavyAlbum.Main
  ( main
  )
where

{-
  Required environment variables:

  - NAVY_ALBUM_REPO: path to the repo.

 -}

import Control.Monad
import Filesystem.Path.CurrentOS hiding (null)
import qualified KcNavyAlbum.DefaultDigest
import qualified KcNavyAlbum.MapBgm
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Environment
import System.Exit hiding (die)
import Turtle.Prelude
import Prelude hiding (FilePath)

main :: IO ()
main = do
  Just fp <- need "NAVY_ALBUM_REPO"
  cd (fromText fp)
  mgr <- newManager tlsManagerSettings

  getArgs >>= \case
    subCmd : args
      | Just handler <- lookup subCmd handlers ->
        withArgs args (handler mgr ("<prog> " <> subCmd <> " "))
    _ -> do
      forM_ handlers $ \(sub, _) ->
        putStrLn $ "<prog> " <> sub <> " ..."
      exitFailure
  where
    handlers =
      [ ("map-bgm", KcNavyAlbum.MapBgm.subCmdMain)
      , ("default-digest", KcNavyAlbum.DefaultDigest.subCmdMain)
      ]
