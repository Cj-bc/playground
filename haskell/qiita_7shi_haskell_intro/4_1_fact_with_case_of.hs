fact = \n -> case n of
  0 -> 1
  _ | n > 0 -> n * fact (n - 1)

main = do
  print $ fact 5
