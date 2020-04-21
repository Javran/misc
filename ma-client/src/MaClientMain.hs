{-# LANGUAGE
    OverloadedStrings
  , TypeApplications
  #-}
module MaClientMain
  ( main
  ) where

import Data.Bits
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import Data.ProtoLens
import System.Environment
import Data.Word
import Lens.Micro

import qualified Proto.MatchingAgent as MA
import qualified Proto.MatchingAgent_Fields as MA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- keep receiving until get the desired length.
recvAtLeast :: Socket -> Int -> IO BS.ByteString
recvAtLeast s todoCount = do
  payload <- recv s todoCount
  let remaining = todoCount - BS.length payload
  if payload == "" || remaining == 0
    then pure payload
    else (payload <>) <$> recvAtLeast s remaining

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
        $ [len, len `shiftR` 8, len `shiftR` 16, len `shiftR` 24] -- TODO: endianness is still assumed for now.
  addr:_ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just "17151")
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
  connect sock $ addrAddress addr
  sendAll sock encodedLen
  sendAll sock encoded
  putStrLn $ "Sent raw: " <> show (BS.length raw)
  [r0, r1, r2, r3] <- fmap (fromIntegral @Word8 @Word32) . BS.unpack <$> recvAtLeast sock 4
  let responseSize = r0 .|. (r1 `shiftL` 8) .|. (r2 `shiftL` 16) .|. (r3 `shiftL` 24) -- TODO: endianness
  responseRaw <- recvAtLeast sock (fromIntegral responseSize)
  let responseM :: Either String MA.FindTagResponse
      responseM = decodeMessage responseRaw
  case responseM of
    Left err -> putStrLn $ "Decode error: " <> err
    Right msg ->
      putStrLn . showMessage $ msg
  close sock
  pure ()
