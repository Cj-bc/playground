fizzbuzz :: Int -> String
fizzbuzz 1             = show 1
fizzbuzz max
  | (mod max 15) == 0 = (fizzbuzz (max-1)) ++ " " ++ "fizzbuzz"
  | (mod max 5) == 0  = (fizzbuzz (max-1)) ++ " " ++ "buzz"
  | (mod max 3) == 0  = (fizzbuzz (max-1)) ++ " " ++ "fizz"
  | otherwise = (fizzbuzz (max-1)) ++ " " ++ show max

main = do
  putStrLn "number: "
  max <- getLine
  print $ fizzbuzz (read max :: Int)
