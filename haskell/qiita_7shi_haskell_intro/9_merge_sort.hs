merge :: [Int] -> [Int] -> [Int]
merge [] _                      = []
merge _ []                      = []
merge (x:xs) (y:ys) | x < y     = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

main = print $ merge [1,3,5,1,6,4,7,84,6,3]
                     [1,5,3,9,2,20,0,111,49]
