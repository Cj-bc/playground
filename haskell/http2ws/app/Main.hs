{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (void)
import Control.Concurrent.MVar
import Data.Conduit
import Data.ByteString (ByteString)
import Network.Wai (responseLBS, requestMethod)
import Network.Wai.Conduit (sourceRequestBody, Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.Wai.Handler.Warp (run)
import Network.WebSockets 
import Network.HTTP.Types.Status
import Network.HTTP.Types.Method

main = do
  wsConnection <- newEmptyMVar
  run 3126 $ websocketsOr defaultConnectionOptions (wsApp wsConnection) (app wsConnection)


app :: MVar Connection -> Application
app connBox req respond | requestMethod req == methodPost = do
                         conn <- tryReadMVar connBox
                         case conn of
                           Nothing -> do
                             liftIO $ putStrLn "Peer doesn't exist yet"
                             respond $ responseLBS notModified304 [] "Peer doesn't exist yet"
                           Just conn' -> do
                             let sendData txt = liftIO $ case txt of
                                   Nothing -> return ()
                                   Just txt' -> sendTextData conn' txt'
                                 logOutput txt = liftIO $ do
                                   putStrLn "---"
                                   print txt
                                   putStrLn "---"
                             liftIO $ connect (sourceRequestBody req) $ do
                               txt <- await
                               -- sendData txt
                               logOutput txt
                               -- (await >>= \txt -> sendData txt >> logOutput txt)
                             respond $ responseLBS ok200 [] ""
app _ _ respond = do
  liftIO $ putStrLn "POST is only option available"
  respond $ responseLBS methodNotAllowed405 [] ""

-- | Accept Websocket request, and put it to the box
wsApp :: MVar Connection -> ServerApp
wsApp connBox conn = do
  conn' <- acceptRequest conn
  forkPingThread conn' 30
  void $ tryPutMVar connBox conn'
