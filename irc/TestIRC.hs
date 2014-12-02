import Network.IRC
import Network.Socket
import qualified Network.Socket.ByteString as SBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Network.BSD
import Control.Concurrent
import System.IO
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Sequence (ViewL(..),(|>))

unfoldM :: Monad m => (s -> m (Maybe s)) -> s -> m ()
unfoldM f s = f s >>= maybe (return ()) (unfoldM f)

ircOpen :: HostName -> PortNumber -> IO Handle
ircOpen hn pn = do
    s <- socket AF_INET Stream defaultProtocol
    HostEntry { hostAddresses = addr:_ } <- getHostByName hn
    connect s (SockAddrInet pn addr)
    socketToHandle s ReadWriteMode

ircWorker :: Handle -> IO ()
ircWorker h = do
    unfoldM workingLoop ()
  where
    workingLoop :: () -> IO (Maybe ())
    workingLoop _ = do
        eof <- hIsEOF h
        if eof then
          return Nothing
          else do
            d <- BS.hGetLine h
            print (decode d)
            return (Just ())

interactWorker :: Handle -> IO ()
interactWorker h = do
    s <- BSC.getLine
    if s == BSC.pack "quit"
        then do
               hClose h
               return ()
        else do
          BSC.hPutStrLn h s
          interactWorker h

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    h <- ircOpen "irc.freenode.org" 6667
    _ <- forkIO (ircWorker h)
    interactWorker h
