{-# LANGUAGE OverloadedStrings #-}
module Hash.Parser.Parser (
  Parser
, shellScript
) where
import Hash.Parser.Type
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import qualified Control.Monad.Trans.State as StateM

import Cjbc.Data.String (show')

type Parser = ParsecT Void Text (StateM.State Environment)


shellScript :: Parser Token
shellScript = assignment
            <|> ifStatement

-- Some definitions defined by Opengroups {{{

-- | Set of characters that consist 'Name' defined in below url
--
-- https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_235
nameChars = choice . map char $ '_' :['A'..'X'] ++ ['a'..'x'] ++ ['0'..'9']


-- | Set of characters that can be the first letter of 'Name' defined in below URL
--
-- https://pubs.opengroup.org/onlinepubs/9699919799/basedefs/V1_chap03.html#tag_03_235
nameCharsAtStart = choice . map char $ '_' : ['A'..'X'] ++ ['a'..'x']
-- }}


ifStatement :: Parser Token
ifStatement = do
    string $ show' IF
    spaceChar
    cond <- binExpr
    char ';'
    spaceChar
    string $ show' THEN
    thenContent <- shellScript
    char ';'
    spaceChar
    -- TODO
    -- elseContent <- elseStatement
    string $ show' FI

    return $ If cond thenContent Nothing

-- BinaryExprToken {{{
binExpr :: Parser BinaryExprToken
binExpr = andExpr <|> orExpr <|> notExpr

andExpr :: Parser BinaryExprToken
andExpr = do
    l <- binExpr
    string "-a"
    r <- binExpr
    return $ And l r

orExpr :: Parser BinaryExprToken
orExpr = do
    l <- binExpr
    string "-o"
    r <- binExpr
    return $ Or l r

notExpr :: Parser BinaryExprToken
notExpr = do
    string "!"
    Not <$> binExpr
-- }}}


-- | Assignment
--
-- https://pubs.opengroup.org/onlinepubs/9699919799/, 2.10.2 Shell Grammar Rules, 7
assignment :: Parser Token
assignment = do
    name_start <- nameCharsAtStart
    name_rest  <- manyTill nameChars (char '=')
    value      <- manyTill asciiChar eol
    let name = name_start : name_rest
    return $ AssignVariable (T.pack name) (T.pack value)



--          parseBuiltin :: Parser Token
--          parseBuiltin = choice . map string $ builtinWords
--          
--          -- | Parser whole input
--          parseInput :: Parser Token
--          parseInput = parseReserved
--                       <|> parseFunction
--                       <|> parseVariable
--                       <|> parseShStatement
