merge :: [Int] -> [Int] -> [Int]
merge [] x                      = x
merge x []                      = x
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys


msort :: [Int] -> [Int]
msort [x] = [x]
msort x  = merge (msort first) (msort second)
          where
            (first, second) = splitAt ((length x) `div` 2) x

main = print $ msort [3,5,1,6,4,7,84,6,3]
