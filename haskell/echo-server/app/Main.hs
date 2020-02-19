module Main where

import Network.Socket
import Network.Socket.ByteString
import Control.Monad (void, forever)
import Control.Concurrent (forkFinally)

main :: IO ()
main = do
    sock <- createSocket
    listen sock 1
    forever $ forkServerForEach sock


createSocket :: IO Socket
createSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    let hints = defaultHints {
                      addrFamily = AF_INET
                    , addrSocketType = Stream
                }
    addr <- head <$> getAddrInfo (Just hints) Nothing (Just "echo")
    bind sock $ addrAddress addr
    return sock

forkServerForEach :: Socket -> IO ()
forkServerForEach sock = do
    (sock', addr) <- accept sock
    void $ forkFinally (forever $ server sock') (const $ gracefulClose sock' 5000)

server sock = do
    received <- recv sock 1024
    send sock received
