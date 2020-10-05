{-# LANGUAGE GADTs #-}
module Hash.Parser.Type where
import Prelude hiding (lookup)
import Data.Text
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

data Environment = Environment { _variables :: M.Map Text Text
                               , _functions :: M.Map Text [Token]
                   }

-- | Create default Environment.
--
-- All Environment variables will be 
defaultEnvironment :: IO Environment
defaultEnvironment = Environment . M.fromList <$> getEnvironment

data NumericalExprToken where
    Plus  :: (Num a, Show a, Ord a) => a -> a -> NumericalExprToken
    Minus :: (Num a, Show a, Ord a) => a -> a -> NumericalExprToken
    Multi :: (Num a, Show a, Ord a) => a -> a -> NumericalExprToken
    Div   :: (Num a, Show a, Ord a) => a -> a -> NumericalExprToken

instance Show NumericalExprToken
instance Eq NumericalExprToken
instance Ord NumericalExprToken

-- | Presents binary expr
data BinaryExprToken where
    And       :: BinaryExprToken -> BinaryExprToken -> BinaryExprToken
    Or        :: BinaryExprToken -> BinaryExprToken -> BinaryExprToken
    Not       :: BinaryExprToken -> BinaryExprToken
    Statement :: Bool -> BinaryExprToken -- TODO: What the f is this???

instance Show BinaryExprToken where
    show (And l r)     = mconcat [show l, " -a ", show r]
    show (Or l r)      = mconcat [show l, " -o ", show r]
    show (Not c)       = mconcat ["! ", show c]
    show (Statement _) = "[TODO] Statement"


data BuiltinCmd = Set String String -- ^ Set <Name> <Value>
                | Expr NumericalExprToken
    deriving (Eq, Show, Ord)


-- | Represents File Descriptor used in redirect
type FileDescriptor = Int


-- Token {{{
data Token where
    Executable          :: Text -> (V.Vector Text) -> Token -- ^ Executable, Arguments
    ShPipe              :: Token -> Token -> Token
    Builtin             :: BuiltinCmd -> (V.Vector Text) -> Token -- ^ Builtin, Arguments
    RedirectIn          :: FileDescriptor -> FilePath -> Token -- ^edirection of input. see `man bash`
    RedirectOut         :: FileDescriptor -> FilePath -> Token -- ^edirection of output. see `man bash`
    RedirectOutAppend   :: FileDescriptor -> FilePath -> Token -- ^edirection of output but appending. see `man bash`
    ShStatement         :: [Token] -> Token
    AssignVariable      :: Text -> Text -> Token -- ^ Name and Content
    ShExpr              :: BinaryExprToken -> Token   -- ^ Expr used in test command, etc
    If                  :: BinaryExprToken -> Token -> (Maybe Token) -> Token  -- ^ Condition, content of 'THEN', content of 'ELSE'
    Case                :: (M.Map CaseExpr Token) -> Token -- ^ [(regex, content), (regex, content)...]
    AssignFunction      :: Text -> Token -> Token -- ^ name, content of function
    For                 :: (V.Vector a) -> (V.Vector Token) -> Token

instance Show Token where
    show (Executable exe arg)       = mconcat [show exe, foldl (\l r -> l ++ " " ++ (show r)) "" arg]
    show (ShPipe l r)               = mconcat [show l, " | ", show r]
    show (Builtin cmd arg)          = mconcat [show cmd, foldl (\l r -> l ++ " " ++ (show r)) "" arg]
    show (RedirectIn  fd fp)        = mconcat [show fd, "< ",  show fp]
    show (RedirectOut fd fp)        = mconcat [show fd, "> ",  show fp]
    show (RedirectOutAppend fd fp)  = mconcat [show fd, ">> ", show fp]
    show (ShStatement (s:ss))       = mconcat [show s,  "; ", mconcat $ fmap show ss]
    show (AssignVariable name c)    = mconcat ["declare", " -- ", show name, "=", "\"", show c, "\""] -- TODO: support options
    show (ShExpr expr)              = "ShExpr" -- TODO
    show (If cond then_ else_)      = mconcat ["if ", show cond, "; then ", show then_, "; else", maybe "" show else_]
    show (Case cases)               = "case" -- TODO
    show (AssignFunction name cont) = "function declare" -- TODO
    show (For items cont)           = " for fo " -- TODO
-- }}}
--
data ReservedWords = IF | THEN | ELSE | ELIF | FI | DO | DONE
                   | CASE | ESAC | WHILE | UNTIL | FOR
                   | LBRACE | RBRACE | BANG | IN
                    deriving (Eq, Ord)


instance Show ReservedWords where
    show IF     = "if"
    show THEN   = "then"
    show ELSE   = "else"
    show ELIF   = "elif"
    show FI     = "fi"
    show DO     = "do"
    show DONE   = "done"
    show CASE   = "case"
    show ESAC   = "esac"
    show WHILE  = "while"
    show UNTIL  = "until"
    show FOR    = "for"
    show LBRACE = "{"
    show RBRACE = "}"
    show BANG   = "!"
    show IN     = "in"

