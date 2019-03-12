sum' []     = 0
sum' (x:xs) = x + sum' xs

product' []     = 1
product' (x:xs) = x * product' xs

take' _ []         = []       -- return [] whenever given list is empty
take' n _ | n < 1  = []       -- Should consider n<1, not only n==1
take' n (x:xs)     = x ++ take' (n-1) xs

drop' _ []         = []       -- return [] whenever given list is empty
drop' n xs | n < 1 = xs       -- Should consider n<1
drop' n (_:xs)     = drop' (n-1) xs   -- not (x:xs), 'cuz x doesn't used

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
