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

data Token where
    Executable      :: Text -> (V.Vector Text) -> Token -- ^ Executable, Arguments
    ShPipe          :: Token -> Token -> Token
    Builtin         :: BuiltinCmd -> (V.Vector Text) -> Token -- ^ Builtin, Arguments
    Redirect        :: FileDescriptor -> FilePath -> Token
    ShStatement     :: [Token] -> Token
    AssignVariable  :: Text -> Text -> Token -- ^ Name and Content
    ShExpr          :: BinaryExprToken -> Token   -- ^ Expr used in test command, etc
    If              :: BinaryExprToken -> Token -> (Maybe Token) -> Token  -- ^ Condition, content of 'THEN', content of 'ELSE'
    Case            :: (M.Map CaseExpr Token) -> Token -- ^ [(regex, content), (regex, content)...]
    AssignFunction  :: Text -> Token -> Token -- ^ name, content of function
    For             :: (V.Vector a) -> (V.Vector Token) -> Token


