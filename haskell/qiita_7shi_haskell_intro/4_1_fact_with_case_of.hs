fact n = case n of
  0 -> 1
  _ -> n * fact (n - 1)

main = do
  print $ fact 5
