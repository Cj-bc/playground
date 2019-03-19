fact 0 = return 1
fact n | n >0 = do
  r <- fact (n-1)
  return $ n * r

main = do
  print =<< fact 5
