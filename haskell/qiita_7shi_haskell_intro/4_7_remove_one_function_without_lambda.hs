f xs = [ (*) 2 x | x <- xs]

main = do
  print $ f [1..5]
