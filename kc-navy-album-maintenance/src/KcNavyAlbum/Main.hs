module KcNavyAlbum.Main
  ( main
  )
where

import Kantour.GameResource.Magic
import Network.HTTP.Client
import Network.HTTP.Types
import Control.Monad
import Text.Printf

checkBattleBgm :: Manager -> Int -> IO Int
checkBattleBgm mgr bgmId = do
  let code = magicCode bgmId "bgm_battle"
  req <- parseRequest (printf "http://%s/kcs2/resources/bgm/battle/%03d_%04d.mp3" defaultServer bgmId code)
  resp <- httpNoBody req mgr
  pure (statusCode $ responseStatus resp)

main :: IO ()
main = do
  mgr <- newManager defaultManagerSettings
  print =<< forM [1..20] (checkBattleBgm mgr)
