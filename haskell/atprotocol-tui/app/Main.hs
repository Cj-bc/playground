{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, Value)

import Web.ATProto.Lexicons.Com.Atproto.RepoDescribe
import Web.ATProto.Lexicons.TH
$(mkRepoDescribeOutput)

[fromJ|
{
  "lexicon": 1,
  "id": "app.bsky.post",
  "type": "record",
  "key": "tid",
  "record": {
    "type": "object",
    "required": ["text", "createdAt"],
    "properties": {
      "text": {"type": "string", "maxLength": 256},
      "entities": {"$ref": "#/defs/entity"},
      "reply": {
        "type": "object",
        "required": ["root", "parent"],
        "properties": {
          "root": {"$ref": "#/defs/postRef"},
          "parent": {"$ref": "#/defs/postRef"}
        }
      },
      "createdAt": {"type": "string", "format": "date-time"}
    }
  },
  "defs": {
    "postRef": {
      "type": "object",
      "required": ["uri", "cid"],
      "properties": {
        "uri": {"type": "string"},
        "cid": {"type": "string"}
      }
    },
    "entity": {
      "type": "array",
      "items": {
        "type": "object",
        "required": ["index", "type", "value"],
        "properties": {
          "index": {"$ref": "#/defs/textSlice"},
          "type": {
            "type": "string",
            "$comment": "Expected values are 'mention', 'hashtag', and 'link'."
          },
          "value": {"type": "string"}
        }
      }
    },
    "textSlice": {
      "type": "array",
      "items": [{"type": "number"}, {"type": "number"}],
      "minItems": 2,
      "maxItems": 2
    }
  }
}
|]

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
