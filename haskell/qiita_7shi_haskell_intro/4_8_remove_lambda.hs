f1 g = g 1
f2 g = g 2 3

main = do
  print $ f1 $ flip (-) 3
  print $ f1 $ (-) 3
  print $ f2 $ (+)
