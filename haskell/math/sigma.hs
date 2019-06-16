
sigma :: Int -> Int -> (Int -> Int) -> Int
sigma current max fn | current == max = fn current
                     | otherwise      = fn current + sigma (current + 1) max fn


main = do
  putStrLn "sigma 1 -> 20 (2k + 3):"
  print $ sigma 1 20 (\k -> 2 * k + 3)
  putStrLn "sigma 1 -> 5 (k + 3)^2:"
  print $ sigma 1 5 (\k -> (k + 3)^2)

