import Control.Applicative
import Data.Char
import Control.Monad

check :: String -> Maybe String
check s = do
  guard $ length s == 3
  guard $ isDigit $ head s
  guard $ isUpper $ s !! 1
  guard $ isLower $ s !! 2
  Just s
  <|> do
  guard $ length s == 3
  guard $ isUpper $ head s
  guard $ isLower $ s !! 1
  guard $ isLower $ s !! 2
  Just s
  <|> Nothing


main :: IO ()
main = do
  print $ check "1"
  print $ check "2Ab"
  print $ check "Abc"
  print $ check "Ab1"
  print $ check "1AB"
