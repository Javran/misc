{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module MaClientMain
  ( main
  ) where

import Data.Bits
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Data.ProtoLens
import System.Environment
import Data.Word
import Lens.Micro

import qualified Proto.MatchingAgent as MA
import qualified Proto.MatchingAgent_Fields as MA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = do
  [fp] <- getArgs
  raw <- BS.readFile fp
  let hints = defaultHints { addrSocketType = Stream }
      req :: MA.FindTagRequest
      req = defMessage & MA.payload .~ raw
      encoded = encodeMessage req
      len :: Word32
      len = fromIntegral $ BS.length encoded
      encodedLen =
        BS.pack
        . fmap (fromIntegral @Word32 @Word8 . (.&. 0xFF) )
        $ [len, len `shiftR` 8, len `shiftR` 16, len `shiftR` 24]
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "17151")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  sendAll sock encodedLen
  sendAll sock encoded
  putStrLn $ "Sent raw: " <> show (BS.length raw)
  close sock
  pure ()
