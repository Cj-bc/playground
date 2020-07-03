{-# LANGUAGE GADTs #-}
module Hash.Type where

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


data Token = Executable String
           | ShPipe Token Token
           | Builtin BuiltinCmd
           | Redirect FilePath



