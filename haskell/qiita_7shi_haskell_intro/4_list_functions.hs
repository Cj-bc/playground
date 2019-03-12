sum' []     = 0
sum' (x:xs) = x + sum' xs

product' []     = 1
product' (x:xs) = x * product' xs

take' n (x:xs) | n == 1 = x
               | n > 1  = x ++ take' (n-1) xs

drop' n (x:xs) | n == 1 = xs
               | n > 1  = drop' (n-1) xs

reverse' []     = []
reverse' (x:xs) = reverse' xs ++ [x]

main = do
  putStrLn "Various functions for List."
  putStrLn "arg: [0..10]"
  print $ sum' [0..10]
  print $ product' [0..10]
  print $ take' 2 [0..10]
  print $ drop' 2 [0..10]
  print $ reverse' [0..10]
