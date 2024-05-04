{-# LANGUAGE OverloadedStrings #-}
module Parser (preamble) where

import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary
import Control.Monad
import Data.ByteString as BS
import Data.Void
import Data.Word (Word32)

type Parser = Parsec Void BS.ByteString

-- | Parser for WASM preamble
preamble :: Parser Word32
preamble = string "\0asm" *> word32le
