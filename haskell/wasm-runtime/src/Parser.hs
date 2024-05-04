{-# LANGUAGE OverloadedStrings #-}
module Parser (preamble) where

import Text.Megaparsec
import Text.Megaparsec.Byte
import Control.Monad
import Data.ByteString as BS
import Data.Void

type Parser = Parsec Void BS.ByteString

preamble :: Parser ()
preamble = void $ string "\0asm"
