{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value)

import Web.ATProto.Lexicons.Com.Atproto.RepoDescribe
$(mkRepoDescribeOutput)

type DID = Text
data DidDoc = DidDoc { context :: [Text]
                     , alsoKnownAs :: [Text]
                     -- TODO: Unimplemented
                     }

-- {{{ Codes for com.atproto.repoListRecords
type CID = Text
data RepoListRecordsOutputItem = RepoListRecordsOutputItem { uri :: Text
                                                           , cid :: CID
                                                           , value :: Value
                                                           } deriving (Generic, Show)
instance FromJSON RepoListRecordsOutputItem
data RepoListRecordsOutput = RepoListRecordsOutput { cursor :: Maybe Text
                                                   , records :: [RepoListRecordsOutputItem]
                                                   } deriving (Generic, Show)
                                      
instance FromJSON RepoListRecordsOutput
-- }}}
  
main :: IO ()
main = do
  let service = "http://localhost:2583/xrpc/com.atproto.repoDescribe?user=bob.test" { requestHeaders = [("Accept", "application/ld+json")]}
  -- let service = "http://localhost:2583/xrpc/com.atproto.repoListRecords?user=bob.test&collection=app.bsky.post" { requestHeaders = [("Accept", "application/ld+json")]}
  bob'sRepoRes <- responseBody <$> httpJSON service 
  print (bob'sRepoRes :: RepoDescribeOutput)
  return ()
