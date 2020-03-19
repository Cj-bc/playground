{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Parser where
import Data.Text
import Data.Attoparsec.Text
import Control.Applicative ((<|>))

-- JqFilter related {{{
data JqFilter = JqField Text JqFilter
              | JqIndex Int  JqFilter
              | JqNil
    deriving (Show, Eq, Read)


parseJqFilter :: Text -> Either Text JqFilter
parseJqFilter input = showParseResult $ parse (jqFilterParser <* skipSpace <* endOfInput) input `feed` ""


showParseResult :: Show a => Result a -> Either Text a
showParseResult (Done _ r) = Right r
showParseResult r = Left . pack . show $ r

jqFilterParser :: Parser JqFilter
jqFilterParser = schar '.' >> (jqField <|> jqIndex <|> pure JqNil)
    where
      jqFilter :: Parser JqFilter
      jqFilter = (schar '.' >> jqField) <|> jqIndex <|> pure JqNil

      jqField :: Parser JqFilter
      jqField  = JqField <$> word <*> jqFilter

      jqIndex :: Parser JqFilter
      jqIndex  = JqIndex <$> (schar '[' *> decimal <* schar ']') <*> jqFilter

word :: Parser Text
word = fmap pack $ many1 (letter <|> schar '-' <|> schar '_' <|> digit)

schar :: Char -> Parser Char
schar c = skipSpace *> char c <* skipSpace
-- }}}

-- JqQuery related {{{
data JqQuery = JqQueryObject [(Text, JqQuery)]
           | JqQueryArray [JqQuery]
           | JqQueryFilter JqFilter
          deriving (Show, Read, Eq)


parseJqQuery :: Text -> Either Text JqQuery
parseJqQuery input = showParseResult $ parse (jqQueryParser <* endOfInput) input `feed` ""

jqQueryParser :: Parser JqQuery
jqQueryParser = queryArray <|> queryObject <|> queryFilter
  where
      queryArray  = JqQueryArray   <$> (schar '[' *> jqQueryParser `sepBy` (schar ',') <* schar ']')
      queryObject = JqQueryObject <$> (schar '{' >> (qObj `sepBy` (schar ',') <* schar '}'))
      qObj        = (,) <$> (schar '"' *> word <* schar '"') <*> (schar ':' *> jqQueryParser)
      queryFilter = JqQueryFilter <$> jqFilterParser
-- }}}
