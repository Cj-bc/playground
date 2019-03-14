
bswap :: [Int] -> [Int]
bswap [x]       = [x]
bswap (x:y:ys) | x < y      = y:bswap (x:ys)
               | otherwise  = x:bswap (y:ys)

bsort :: [Int] -> [Int]
bsort [x] = [x]
bsort x = bsort (init y) ++ [last y]
  where
    y = bswap x



main = do
  print $ bsort [1,25,2, 0,69, 4, 68, 19]
