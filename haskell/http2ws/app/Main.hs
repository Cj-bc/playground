{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (forever)
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


app :: MVar ByteString -> Application
app txtBox req respond | requestMethod req == methodPost = do
                             connect (sourceRequestBody req) (sendDataSink txtBox)
                             respond $ responseLBS ok200 [] ""
app _ _ respond = do
  liftIO $ putStrLn "POST is only option available"
  respond $ responseLBS methodNotAllowed405 [] ""

sendDataSink :: MonadIO m => MVar ByteString -> ConduitT ByteString Void m ()
sendDataSink txtBox = await >>= (maybe (return ()) (liftIO . putMVar txtBox))

-- | Accept Websocket request, await for receiving text from HTTP server,
-- and send it to the websocket client
wsApp :: MVar ByteString -> ServerApp
wsApp txtBox conn = do
  conn' <- acceptRequest conn
  forever $ do
    txt <- takeMVar txtBox
    sendTextData conn' txt
