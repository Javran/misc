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
import Network.HTTP.Client
import Turtle.Prelude
import Prelude hiding (FilePath)
import System.Environment
import qualified KcNavyAlbum.MapBgm
import System.Exit hiding (die)

main :: IO ()
main = do
  Just fp <- need "NAVY_ALBUM_REPO"
  cd (fromText fp)
  mgr <- newManager defaultManagerSettings

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
      [ ("map-bgm",  KcNavyAlbum.MapBgm.subCmdMain)
      ]
