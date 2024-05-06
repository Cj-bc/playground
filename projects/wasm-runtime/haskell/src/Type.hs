module Type where

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

-- | WASM binary module.
data WasmModule = Module { wasmVersion :: Int
                         , typeSection :: [FuncType]
                         -- , importSection :: [Import]
                         -- , functionSection :: [Function]
                         -- , tableSection :: [Table]
                         -- , memorySection :: [Memory]
                         -- , globalSection :: [Global]
                         -- , exportSection :: [Export]
                         -- , startSection :: [Start]
                         -- , elementSection :: [Element]
                         -- , codeSection :: [Code]
                         -- , dataSection :: [Data]
                         }
  deriving (Show)
