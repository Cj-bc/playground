{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (void, forever)
import Data.ByteString (ByteString(..), append)
import Data.ByteString.UTF8 (fromString)
import Network.Socket
import Network.Socket.ByteString
import System.Random (randomRIO)

main :: IO ()
main = do
    sock <- createSocket
    listen sock 1
    forever $ forkServerForEach sock


createSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    let hints = defaultHints {
                      addrFamily = AF_INET
                    , addrSocketType = Stream
                    }
    addr <- head <$> getAddrInfo (Just hints) Nothing (Just "qotd")
    bind sock $ addrAddress addr
    return sock

forkServerForEach :: Socket -> IO ()
forkServerForEach sock = do
    (sock', addr) <- accept sock
    -- Create new forked process for one socket
    void $ forkFinally (server sock') (const $ gracefulClose sock' 5000)

server sock = do
    recieved <- recv sock 1024
    print recieved
    quote <- pickRandomQuote
    send sock $ quote `append` "\r\n"


pickRandomQuote :: IO ByteString
pickRandomQuote = do
    quotes <- lines <$> readFile "resources/quotes.txt"
    pickedIdx <- randomRIO (0, length quotes - 1)
    return $ fromString $ quotes !! pickedIdx
