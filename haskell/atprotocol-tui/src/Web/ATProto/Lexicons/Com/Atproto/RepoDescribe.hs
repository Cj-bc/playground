{-# LANGUAGE TemplateHaskell #-}
module Web.ATProto.Lexicons.Com.Atproto.RepoDescribe where
import GHC.Generics (Generic)
import Data.Text (Text)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Quasi)

mkRepoDescribeOutput :: Quasi m => m [Dec]
mkRepoDescribeOutput = return [DataD [] (mkName "RepoDescribeOutput") []
                        Nothing [RecC (mkName "RepoDescribeOutput")
                                 [(mkName "name", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
                                 , (mkName "did", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Text)
                                 , (mkName "collections", Bang NoSourceUnpackedness NoSourceStrictness, AppT ListT (ConT ''Text))
                                 , (mkName "nameIsCorrect", Bang NoSourceUnpackedness NoSourceStrictness, ConT ''Bool)]]
                        [DerivClause Nothing [ConT ''Generic, ConT ''Show]]]


