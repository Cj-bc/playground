fib :: Int -> Int
fib n | n == 0    = 0
      | n == 1    = 1
      | otherwise = fib(n-1) + fib(n-2)

main = do
  putStrLn "Fibonacci with guard."
  putStrLn "Number: "
  num <- getLine
  print $ fib (read num :: Int)
