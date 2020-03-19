{-# LANGUAGE OverloadedStrings #-}
import Test.HUnit
import Data.Hjq.Parser
import Data.Hjq.Query
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.Lens
import Lens.Micro ((^?))
import qualified Data.HashMap.Strict as H
import qualified Data.Vector as V
import qualified Data.Text as T


main :: IO ()
main = void $ runTestTT $ TestList
        [ jqFilterParserTest
        , jqFilterParserSpaceTest
        , jqQueryParserTest
        , applyFilterTest
        , executeQueryTest
        ]

-- testData {{{
testData :: Value
testData = Object $ H.fromList
    [ ("string-field", String "string value")
    , ("nested-field", Object $ H.fromList
            [ ("inner-string", String "inner value")
            , ("inner-number", Number 1000)
            ]
      )
    , ("array-field", Array $ V.fromList
            [ String "first field"
            , String "second field"
            , Object (H.fromList
                [("object-in-array", String "string value in object-in-array")])
            ]
      )
    ]
-- }}}

-- Parse.hs {{{
-- JqFilter {{{2
jqFilterParserTest :: Test
jqFilterParserTest = TestList
    [ "jqFilterParserTest 1: root parsing"      ~: parseJqFilter "."              ~?= Right JqNil
    , "jqFilterParserTest 2: index parsing"     ~: parseJqFilter ".[0]"           ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParserTest 3: fieldName parsing" ~: parseJqFilter ".fieldName"     ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParserTest 4: complicated filter"
        ~: parseJqFilter ".[0].fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParserTest 5: complicated filter"
        ~: parseJqFilter ".fieldName[0]"  ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    ]

jqFilterParserSpaceTest :: Test
jqFilterParserSpaceTest = TestList
    [ "jqFilterParserSpaceTest 1: root parsing"      ~: parseJqFilter " . "           ~?= Right JqNil
    , "jqFilterParserSpaceTest 2: index parsing"     ~: parseJqFilter " . [0] "       ~?= Right (JqIndex 0 JqNil)
    , "jqFilterParserSpaceTest 3: fieldName parsing" ~: parseJqFilter " . fieldName"  ~?= Right (JqField "fieldName" JqNil)
    , "jqFilterParserSpaceTest 4: complicated filter"
        ~: parseJqFilter " . [ 0 ] . fieldName" ~?= Right (JqIndex 0 (JqField "fieldName" JqNil))
    , "jqFilterParserSpaceTest 5: complicated filter"
        ~: parseJqFilter " . fieldName [ 0 ]"  ~?= Right (JqField "fieldName" (JqIndex 0 JqNil))
    , "jqFilterParserSpaceTest 6: skip space at the end" ~: parseJqFilter ". " ~?= Right JqNil
    ]
-- }}}

-- JqQuery {{{2
jqQueryParserTest :: Test
jqQueryParserTest = TestList
    [ "jqQueryParserTest 1"    ~: parseJqQuery "[]"  ~?= Right (JqQueryArray [])
    , "jqQueryParserTest 2"
        ~: parseJqQuery "[.foo, .bar]"
            ~?= Right (JqQueryArray [JqQueryFilter (JqField "foo" JqNil)
                                    , JqQueryFilter (JqField "bar" JqNil)])
    , "jqQueryParserTest 3" ~: parseJqQuery "{\"foo\": [], \"bar\": []}"
            ~?= Right (JqQueryObject [("foo", JqQueryArray []), ("bar", JqQueryArray [])])
    ]

jqQueryParserSpaceTest :: Test
jqQueryParserSpaceTest = TestList
    [ "jqQueryParserSpaceTest 1"    ~: parseJqQuery "[ ]"  ~?= Right (JqQueryArray [])
    , "jqQueryParserSpaceTest 2"
        ~: parseJqQuery "[ . foo , . bar ]"
            ~?= Right (JqQueryArray [JqQueryFilter (JqField "foo" JqNil)
                                    , JqQueryFilter (JqField "bar" JqNil)])
    , "jqQueryParserSpaceTest 3" ~: parseJqQuery "{ \"foo\" : [ ] , \"bar\" : [ ] } "
            ~?= Right (JqQueryObject [("foo", JqQueryArray []), ("bar", JqQueryArray [])])
    ]
-- }}}
-- }}}

-- Query.hs {{{
-- applyFilter {{{2
applyFilterTest :: Test
applyFilterTest = TestList
    [ "applyFilterTest 1" ~: (applyFilter (unsafeParseFilter ".") testData) ~?= Right testData
    , "applyFilterTest 2" ~: (Just $ applyFilter (unsafeParseFilter ".string-field") testData)
            ~?= fmap Right (testData^?key "string-field")
    , "applyFilterTest 3" ~: (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-string") testData)
            ~?= fmap Right (testData^?key "nested-field".key "inner-string")
    , "applyFilterTest 4" ~: (Just $ applyFilter (unsafeParseFilter ".nested-field.inner-number") testData)
            ~?= fmap Right (testData^?key "nested-field".key"inner-number")
    , "applyFilterTest 5" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[0]") testData)
            ~?= fmap Right (testData^?key "array-field" . nth 0)
    , "applyFilterTest 6" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[1]") testData)
            ~?= fmap Right (testData^?key "array-field" . nth 1)
    , "applyFilterTest 7" ~: (Just $ applyFilter (unsafeParseFilter ".array-field[2].object-in-array") testData)
            ~?= fmap Right (testData^?key "array-field" . nth 2 . key "object-in-array")
    ]

unsafeParseFilter :: T.Text -> JqFilter
unsafeParseFilter t = case parseJqFilter t of
                        Right a -> a
                        Left e  -> error $ "PARSE FAILED IN A TEST: " ++ T.unpack e
-- }}}

-- executeQuery {{{2
executeQueryTest :: Test
executeQueryTest = TestList
    [ "executeQuery test 1" ~: executeQuery (unsafeParseQuery "{}") testData ~?= Right (Object $ H.fromList [])
    , "executeQuery test 2" ~: executeQuery (unsafeParseQuery "{\"field1\": ., \"field2\": .string-field}") testData
        ~?= Right (Object $ H.fromList [("field1", testData), ("field2", String "string value")])
    , "executeQuery test 3" ~: executeQuery (unsafeParseQuery "[.string-field, .nested-field.inner-string]") testData
        ~?= Right (Array $ V.fromList [String "string value", String "inner value"])
    ]

unsafeParseQuery :: T.Text -> JqQuery
unsafeParseQuery t = case parseJqQuery t of
                        Right q -> q
                        Left s -> error $ "PARSE FAILURE IN A TEST: " ++ T.unpack s
-- }}}
-- }}}
