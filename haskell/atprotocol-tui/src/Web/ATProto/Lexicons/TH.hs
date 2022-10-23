{-# LANGUAGE OverloadedStrings #-}
module Web.ATProto.Lexicons.TH where
import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BS
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter(QuasiQuoter), quoteDec)
import Data.Aeson (FromJSON, Value(Object, String, Number), Object, eitherDecode)
import qualified Data.Aeson.KeyMap as KM
import Control.Monad (when)
import Data.Either.Extra (maybeToEither)
import Data.Either (fromRight)

fromJ :: QuasiQuoter
fromJ = QuasiQuoter { quoteDec = mkLexiconType
                                 . fromRight (error "Invalid json format.")
                                 . eitherDecode . BS.pack
                    }

-- | Construct necesary 'Dec's from Lexicon JSON data.
-- This __will__ throw error when something went wrong.
mkLexiconType :: Value -> Q [Dec]
mkLexiconType = either error id . mkLexiconType'

-- | Safe version of 'mkLexiconType'
--
-- Todo: What to do with description, revision, and defs fields? They're ignored for now
mkLexiconType' :: Value -> Either String (Q [Dec])
mkLexiconType' (Object o) = do
  -- version check
  lexiconVersion <- maybeToEither "Top-level dictionary should contain 'lexicon'" $ KM.lookup "lexicon" o
  when (lexiconVersion /= (Number 1)) $ Left "only lexicon version 1 is supported"

  lexiconType <- maybeToEither "Top-level dictionary should contain 'type'" $ KM.lookup "type" o
  case lexiconType of
    (String "query") -> mkLexiconTypeQuery o
    (String "procedure") -> mkLexiconTypeProcedure o
    (String "record") -> mkLexiconTypeRecord o
mkLexiconType' _ = Left "Top-level should be Object"

-- | Construct 'Q [Dec]' for Query XRPC method
mkLexiconTypeQuery :: Object -> Either String (Q [Dec])
mkLexiconTypeQuery o = Right (return [])
  
-- | Construct 'Q [Dec]' for Precedure XRPC method
mkLexiconTypeProcedure :: Object -> Either String (Q [Dec])
mkLexiconTypeProcedure o = Right (return [])

-- | Construct 'Q [Dec]' for Record definition
--
-- Usage of 'key' field isn't written on document, so I simply ignoreing it for now.
mkLexiconTypeRecord :: Object -> Either String (Q [Dec])
mkLexiconTypeRecord o = do
  let lexiconKey = KM.lookup "key" o
  lexiconId <- maybeToEither "Top-level dictionary should contain 'id'" $ KM.lookup "id" o
  case lexiconId of
    (String lexiconId') -> 
      let nsidName = T.unpack . T.toTitle $ getNsidName lexiconId'
          -- record = may
      in Right $ do
        let recordName = mkName nsidName
        return [DataD [] recordName [] Nothing [RecC recordName []] []]
    _ -> Left "id should be String"

  
getNsidName :: Text -> Text
getNsidName = last . T.splitOn "."
