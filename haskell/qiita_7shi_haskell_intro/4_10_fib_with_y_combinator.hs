y f = f (y f)

main = do
  print $ flip y 10 $
    \fib x -> case x of
      1 -> 1
      2 -> 1
      _ -> fib (x - 1) + fib (x - 2)
