import Network
import Control.Monad
import System.Environment
import Text.Printf
import System.IO
import Control.Concurrent

doEchoServer :: Handle -> IO ()
doEchoServer h = do
    hPutStrLn h "Welcome from Haskell!"
    hGetLine h >>= doEchoServer'
    where
        doEchoServer' s =
            if take 3 s == "bye"
               then do hPutStrLn h "Bye!"
                       hClose h
               else do hPutStrLn h s
                       hGetLine h >>= doEchoServer'

sockHandler :: Socket -> IO ()
sockHandler s = do
    (h,host,port) <- accept s
    hSetBuffering h NoBuffering
    putStrLn $ printf "Connected accepted, host: %s, port: %s\n" (show host) (show port)
    void $ forkIO $ doEchoServer h

main :: IO ()
main = withSocketsDo $ do
    port <- liftM (read . head) getArgs :: IO Int
    sock <- listenOn $ PortNumber (fromIntegral port)
    _ <- printf "Listerning on %d\n" port
    forever $ sockHandler sock
