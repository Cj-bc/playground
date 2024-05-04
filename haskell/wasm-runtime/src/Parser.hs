{-# LANGUAGE OverloadedStrings #-}
module Parser (
  preamble
, wasmModule
) where

import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary
import Control.Monad
import Data.ByteString as BS
import Data.Void
import Data.Word (Word32)

type Parser = Parsec Void BS.ByteString

data WasmModule = Module { version :: Int
                         }
  deriving (Show)

wasmModule :: Parser WasmModule
wasmModule = Module <$> (fmap (fromInteger . toInteger) preamble)

-- | Parser for WASM preamble
preamble :: Parser Word32
preamble = string "\0asm" *> word32le
