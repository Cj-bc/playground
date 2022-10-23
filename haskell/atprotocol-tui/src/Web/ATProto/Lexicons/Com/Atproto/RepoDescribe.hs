{-# LANGUAGE TemplateHaskell #-}
module Web.ATProto.Lexicons.Com.Atproto.RepoDescribe where
import GHC.Generics (Generic)
import Data.Text (Text)
import Language.Haskell.TH
import Data.Aeson (FromJSON)

mkRepoDescribeOutput :: Quote m => m [Dec]
mkRepoDescribeOutput =
  let typeName = mkName "RepoDescribeOutput"
      dataTypeDecl = DataD [] typeName []
                     Nothing [RecC typeName
                              [(mkName "name", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
                              , (mkName "did", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
                              , (mkName "collections", Bang NoSourceUnpackedness NoSourceStrictness, AppT ListT (ConT ''Text))
                              , (mkName "nameIsCorrect", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Bool)]]
           [DerivClause Nothing [ConT ''Generic, ConT ''Show]]
      fromJsonInstance = InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT typeName)) []
  in return [dataTypeDecl, fromJsonInstance]


