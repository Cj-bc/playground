{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main (main) where
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)

type DID = Text
data DidDoc = DidDoc { context :: [Text]
                     , alsoKnownAs :: [Text]
                     -- TODO: Unimplemented
                     }
                     
-- {{{ Codes for com.atproto.repoDescribe
data RepoDescribeOutput = RepoDescribeOutput { name :: Text
                                             , did :: DID
                                             -- Ignore DID Document for now bec I don't know precise type definition
                                             -- , didDoc :: DidDoc
                                             , collections :: [Text]
                                             , nameIsCorrect :: Bool
                                             } deriving (Generic, Show)
instance FromJSON RepoDescribeOutput
-- }}}
main :: IO ()
main = do
  let service = "http://localhost:2583/xrpc/com.atproto.repoDescribe?user=bob.test" { requestHeaders = [("Accept", "application/ld+json")]}
  bob'sRepoRes <- responseBody <$> httpJSON service 
  print (bob'sRepoRes :: RepoDescribeOutput)
  return ()
