{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import Network.HTTP.Simple
import Network.HTTP.Client

main :: IO ()
main = do
  let service = "http://localhost:2583/xrpc/com.atproto.repoDescribe?user=bob.test" { requestHeaders = [("Accept", "application/ld+json")]}
  bob'sRepoRes <- responseBody <$> httpLBS service
  print bob'sRepoRes
  return ()
