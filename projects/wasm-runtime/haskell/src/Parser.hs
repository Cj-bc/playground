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
import Codec.LEB128
import Codec.LEB128.Constraints
import Type (WasmModule (Module), FuncType(..), ValType(..))

type Parser = Parsec Void BS.ByteString

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
wasmModule = do
  string "\0asm"
  version <- fromInteger . toInteger <$> word32le
  types <- typeSection
  return $ Module version types

-- | Parser for WASM preamble
preamble :: Parser Word32
preamble = string "\0asm" *> word32le

-- | Parser for Type Section
typeSection :: Parser [FuncType]
typeSection = do
  _ <- satisfy (== 0x01)
  size <- leb128 :: Parser Word32
  vectorOf funcType

funcType :: Parser FuncType
funcType = do
  _ <- satisfy (== 0x60)
  params <- vectorOf valType
  res <- vectorOf valType
  return $ FuncType params res

valType :: Parser ValType
valType = do
  b <- word8
  case b of
    0x7F -> return I32
    0x7E -> return I64
    0x7D -> return F32
    0x7C -> return F64
    _    -> fail $ "Invalid valueType: " ++ show b

-- | Parser for LEB128 encoded unsigned numbers
leb128 :: LEB128 a => Parser a
leb128 = do
  input <- getInput
  case fromULEB128ByteString input of
    (Nothing, _) -> fail "Invalid LEB128"
    (Just n, bs) -> do
      setInput bs
      return n

-- | Parse wasm vector encoding of some value
vectorOf :: Parser a -> Parser [a]
vectorOf content = flip count content =<< (fromInteger . toInteger <$> (leb128 :: Parser Word32))
