import Data.Char

rot13 :: String -> String
rot13 (x:xs) | xs == []  = [shifted]
             | otherwise = shifted : rot13 xs
  where
    shifted = chr (origin+13)
    origin  = ord x

main = print $ rot13 "abc"
