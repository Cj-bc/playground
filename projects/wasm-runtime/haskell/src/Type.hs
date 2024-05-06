module Type where
import qualified Data.ByteString as BS
import Data.Word (Word32)

-- | Represents WASM function type definition
--
-- https://www.w3.org/TR/wasm-core-1/#function-types①
data FuncType = FuncType { parameters :: [ValType]
                         , results :: [ValType]
                         }
  deriving (Show)

-- | Represents WASM Value type definition
--
-- https://www.w3.org/TR/wasm-core-1/#value-types①
data ValType = I32 | I64 | F32 | F64
  deriving (Show)

data FunctionLocalVar = FunctionLocalVar { countOfLocals :: Int
                                         , valueType :: ValType
                                         }
  deriving (Show)

data Code = Code { locals :: [FunctionLocalVar]
                 -- | Temporary Stored as ByteString until Instrs are implemented
                 , exprs :: BS.ByteString
                 }
  deriving (Show)

type TypeIndex = Word32

-- | WASM binary module.
data WasmModule = Module { wasmVersion :: Int
                         , customSection :: [BS.ByteString]
                         , typeSection :: [FuncType]
                         -- , importSection :: [Import]
                         , functionSection :: [TypeIndex]
                         -- , tableSection :: [Table]
                         -- , memorySection :: [Memory]
                         -- , globalSection :: [Global]
                         -- , exportSection :: [Export]
                         -- , startSection :: [Start]
                         -- , elementSection :: [Element]
                         , codeSection :: [Code]
                         -- , dataSection :: [Data]
                         }
  deriving (Show)
