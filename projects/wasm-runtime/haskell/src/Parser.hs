{-# LANGUAGE OverloadedStrings #-}
module Parser (
  preamble
, wasmModule
) where

import Text.Megaparsec
import Text.Megaparsec.Byte
import Text.Megaparsec.Byte.Binary
import Control.Monad
import qualified Data.ByteString as BS
import Data.Void
import Data.Word (Word32)

type Parser = Parsec Void BS.ByteString

data WasmModule = Module { version :: Int
                         , sections :: [Section]
                         }
  deriving (Show)

data Section = CustomSection
             | TypeSection
             | ImportSection
             | FunctionSection
             | TableSection
             | MemorySection
             | GlobalSection
             | ExportSection
             | StartSection
             | ElementSection
             | CodeSection
             | DataSection
             deriving (Show)

wasmModule :: Parser WasmModule
wasmModule = Module <$> (fmap (fromInteger . toInteger) preamble) <*> (many section)

-- | Parser for WASM preamble
preamble :: Parser Word32
preamble = string "\0asm" *> word32le

section :: Parser Section
section = do
  code <- word8
  size <- word32le
  _ <- count (fromInteger . toInteger $ size) word8
  case code of
    0x00 -> return CustomSection
    0x01 -> return TypeSection
    0x02 -> return ImportSection
    0x03 -> return FunctionSection
    0x04 -> return TableSection
    0x05 -> return MemorySection
    0x06 -> return GlobalSection
    0x07 -> return ExportSection
    0x08 -> return StartSection
    0x09 -> return ElementSection
    0x0a -> return CodeSection
    0x0b -> return DataSection
    _ -> fail "invalid section code"
