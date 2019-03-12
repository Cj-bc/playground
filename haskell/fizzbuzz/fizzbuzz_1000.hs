fizzbuzz :: Int -> String
fizzbuzz 1 = "1"
fizzbuzz n | (mod n 15) == 0 = (fizzbuzz(n-1)) ++ " " ++ "fizzbuzz"
           | (mod n 5)  == 0 = (fizzbuzz(n-1)) ++ " " ++ "buzz"
           | (mod n 3)  == 0 = (fizzbuzz(n-1)) ++ " " ++ "fizz"
           | otherwise       = (fizzbuzz(n-1)) ++ " " ++ show n


main = print $ fizzbuzz(1000)
