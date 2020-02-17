{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : Main
Description : main module for qotd-hs
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

References:

  - https://tools.ietf.org/html/rfc865

  - https://hackage.haskell.org/package/network-3.1.1.1/docs/Network-Socket.html

  - http://agtn.hatenablog.com/entry/2015/07/23/214344
-}
module Main where

import Control.Concurrent (forkFinally)
import Control.Monad (void, forever)
import Data.ByteString (ByteString, append)
import Data.ByteString.UTF8 (fromString)
import Network.Socket
import Network.Socket.ByteString
import System.Random (randomRIO)

main :: IO ()
main = do
    sock <- createSocket
    listen sock 1
    forever $ forkServerForEach sock


-- | create socket
--
-- IPv4, TCP
createSocket = do
    sock <- socket AF_INET Stream defaultProtocol
    let hints = defaultHints {
                      addrFamily = AF_INET -- IPv4
                    , addrSocketType = Stream -- TCP
                    }

    -- Create this in order to automatically calculate 'SockAddr'
    addr <- head <$> getAddrInfo (Just hints) Nothing (Just "qotd")
    bind sock $ addrAddress addr
    return sock

forkServerForEach :: Socket -> IO ()
forkServerForEach sock = do
    (sock', addr) <- accept sock
    -- Create new forked process for one socket
    void $ forkFinally (server sock') (const $ gracefulClose sock' 5000)

--
-- | Actual server to receive request & send response
server sock = do
    recieved <- recv sock 1024  -- Receive 1024 bites from sock
    quote <- pickRandomQuote
    send sock $ quote `append` "\r\n"


-- | Picking up random quote from file
pickRandomQuote :: IO ByteString
pickRandomQuote = do
    quotes <- lines <$> readFile "resources/quotes.txt"
    pickedIdx <- randomRIO (0, length quotes - 1)
    return $ fromString $ quotes !! pickedIdx
