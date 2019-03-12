fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
  putStrLn "Fibonacci"
  putStrLn "number: "
  num <- getLine
  print $ fib (read num :: Int)
