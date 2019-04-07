flip' :: (a -> a -> a) -> a -> a -> (a -> a -> a)
flip' fn first second = fn second first

map' :: (a -> a) -> [a] -> [a]
map' fn xs = [fn x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' fn xs = [x | x <- xs, fn x]

-- not yet
-- foldl' (\x -> \y -> x^y) 2 [1,2,3,4,5]
-- (foldl' (\x -> \y -> x^y) 2 [1,2,3,4])^5
-- ((foldl' (\x -> \y -> x^y) 2 [1,2,3])^4)^5
-- (((foldl' (\x -> \y -> x^y) 2 [1,2])^3)^4)^5
-- ((((foldl' (\x -> \y -> x^y) 2 [1])^2)^3)^4)^5
-- (((((foldl' (\x -> \y -> x^y) 2 [])^1)^2)^3)^4)^5
-- (((((2)^1)^2)^3)^4)^5
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' fn ini []      = ini
foldl' fn ini xs  = fn (foldl' fn ini rest) lx
    where
        lx = last xs
        rest = init xs


-- not yet
-- foldr (\x -> \y -> x^y) 2 [1,2,3,4,5]
--  1^(foldr (\x -> \y -> x^y) 2 [2,3,4,5])
--  1^(2^(foldr (\x -> \y -> x^y) 2 [3,4,5]))
--  1^(2^(3^(foldr (\x -> \y -> x^y) 2 [4,5])))
--  1^(2^(3^(4^(foldr (\x -> \y -> x^y) 2 [5]))))
--  1^(2^(3^(4^(5^(foldr (\x -> \y -> x^y) 2 [])))))
--  1^(2^(3^(4^(5^(2)))))
foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' fn ini [] = ini
foldr' fn ini xs = fn first (foldr' fn ini rest)
    where
        first = head xs
        rest  = drop 1 xs


main = do
  print $ map' (* 2) [1..5]
  print $ filter' (< 5) [1..9]
  print $ flip' map' [1..5] (* 2)
  print $ foldl' (+) 0 [1..100]
  print $ foldl' (-) 0 [1..5]
  print $ foldr' (-) 0 [1..5]
