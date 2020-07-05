{-# LANGUAGE GADTs #-}
module Hash.Parser.Type where

data NumericalExprToken where
    Plus  :: Num a => a -> a -> NumericalExprToken
    Minus :: Num a => a -> a -> NumericalExprToken
    Multi :: Num a => a -> a -> NumericalExprToken
    Div   :: Num a => a -> a -> NumericalExprToken

-- | Presents binary expr
data BinaryExprToken where
    And       :: BinaryExprToken -> BinaryExprToken -> BinaryExprToken
    Or        :: BinaryExprToken -> BinaryExprToken -> BinaryExprToken
    Not       :: BinaryExprToken -> BinaryExprToken
    Statement :: Bool -> BinaryExprToken



data BuiltinCmd = Set String String -- ^ Set <Name> <Value>
                | Expr NumericalExprToken


-- | Represents File Descriptor used in redirect
type FileDescriptor = Int

data Token = Executable Text (V.Vector Text) -- ^ Executable, Arguments
           | ShPipe Token Token
           | Builtin BuiltinCmd (V.Vector Text) -- ^ Builtin, Arguments
           | Redirect FileDescriptor FilePath
           | ShStatement [Token]
           | AssignVariable Text Text -- ^ Name and Content


