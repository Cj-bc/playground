fib 0 = return 0
fib 1 = return 1
fib n | n > 1 = (+) <$> fib (n -2) <*> fib (n -1)

main = do
  print =<< fib 6
