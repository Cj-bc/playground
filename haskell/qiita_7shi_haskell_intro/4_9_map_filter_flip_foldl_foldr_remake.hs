flip' :: (a -> a -> a) -> a -> a -> (a -> a -> a)
flip' fn first second = fn second first

map' :: (a -> a) -> [a] -> [a]
map' fn xs = [fn x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' fn xs = [x | x <- xs, fn x]

-- not yet
foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' fn ini []      = ini
foldl' fn ini (x:xs)  = foldl' fn done xs
    where
        done = fn ini x

-- not yet
foldr' :: (a -> a -> a) -> a -> [a] -> a
foldr' fn ini xs = foldr' fn done xs'
    where
        last' = last xs
        done  = fn ini last'
        xs'   = 
