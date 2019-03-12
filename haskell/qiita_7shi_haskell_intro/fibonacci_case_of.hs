fib n = case n of
          0 -> 0
          1 -> 1
          _ | n > 0 -> fib (n-1) + fib (n-2)

main = do
  putStrLn "Fibonacci with case-of."
  putStrLn "Number: "
  num <- getLine
  print $ fib (read num :: Int)
