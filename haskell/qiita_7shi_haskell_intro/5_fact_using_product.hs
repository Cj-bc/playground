fact :: Int -> Int
fact n = product [1..n]

main = do
  putStrLn "Fact using product."
  putStrLn "fact 10:"
  print $ fact 10
