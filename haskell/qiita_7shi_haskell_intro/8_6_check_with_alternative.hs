import Control.Applicative
import Data.Char
import Control.Monad

check :: String -> Maybe String
check s = do
-- More than 3 letters are accepted.
-- And it's better to test this once.
  guard $ length s >= 3
  do
    guard $ isDigit $ head s
    guard $ isUpper $ s !! 1
    <|> do
    guard $ isUpper $ head s
    guard $ isLower $ s !! 1
-- Last check is the same, so do it out of <|>
  guard $ isLower $ s !! 2
  Just s
--  We don't have to give Nothing here
--  <|> Nothing


main :: IO ()
main = do
  print $ check "1"
  print $ check "2Ab"
  print $ check "Abc"
  print $ check "Ab1"
  print $ check "1AB"
