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
import Data.Maybe (catMaybes)
import Data.Void
import Data.Word (Word32, Word8)
import Codec.LEB128
import Codec.LEB128.Constraints
import Type (WasmModule (Module), FuncType(..), ValType(..)
            , TypeIndex, FunctionLocalVar(..), Code(Code))

type Parser = Parsec Void BS.ByteString

wasmModule :: Parser WasmModule
wasmModule = do
  string "\0asm"
  version <- fromInteger . toInteger <$> word32le
  ct1 <- optional customSection
  types <- typeSection
  ct2 <- optional customSection
  funcs <- functionSection
  ct3 <- optional customSection
  codes <- codeSection
  ct4 <- optional customSection
  return $ Module version (catMaybes [ct1, ct2, ct3, ct4]) types funcs codes

-- | Parser for WASM preamble
preamble :: Parser Word32
preamble = string "\0asm" *> word32le

customSection :: Parser BS.ByteString
customSection = do
  _ <- satisfy (== 0x00)
  size <- (fromInteger . toInteger <$> (leb128 :: Parser Word32)) :: Parser Int
  restInput <- getInput
  let (content, rest) = BS.splitAt size restInput
  setInput rest
  return content

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

functionSection :: Parser [TypeIndex]
functionSection = do
  _ <- satisfy (== 0x03)
  _ <- leb128 :: Parser Word32
  vectorOf typeIndex

typeIndex :: Parser TypeIndex
typeIndex = leb128

codeSection :: Parser [Code]
codeSection = do
  _ <- satisfy (== 0x0a)
  _ <- (fromInteger . toInteger <$> (leb128 :: Parser Word32)) :: Parser Int
  vectorOf code

-- | Parse 'code' part of code section
code :: Parser Code
code = do
  _ <- (fromInteger . toInteger <$> (leb128 :: Parser Word32)) :: Parser Int
  Code <$> vectorOf localVariable <*> expr

-- TODO: Implement instr parsing once those operations are implemented.
expr :: Parser BS.ByteString
expr = do
  inst <- word8
  BS.pack <$> go inst []
  where
    go :: Word8 -> [Word8] -> Parser [Word8]
    go 0x0b acc = return acc
    go instr acc = do
      next <- word8
      go next (instr:acc)

localVariable :: Parser FunctionLocalVar
localVariable = FunctionLocalVar <$> (fromInteger . toInteger <$> (leb128 :: Parser Word32)) <*> valType

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
