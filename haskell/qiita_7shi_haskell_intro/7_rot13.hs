import Data.Char


rot13ch :: Char -> Char
rot13ch ch |  'A' <= ch && ch <= 'M'
           || 'a' <= ch && ch <= 'm' = chr $ ord ch +13
           |  'N' <= ch && ch <= 'Z'
           || 'n' <= ch && ch <= 'z' = chr $ ord ch -13
           | otherwise               = ch

rot13 :: String -> String
rot13 ""      = ""
rot13 (x:xs)  =  rot13ch x : rot13 xs

main = print $ rot13 "abc"
