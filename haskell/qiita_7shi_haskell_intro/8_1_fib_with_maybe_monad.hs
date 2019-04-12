import Control.Applicative

fib 0 = Just 0
fib 1 = Just 1
fib n | n> 1      = (+) <$> fib (n - 2) <*> fib (n - 1)
      | otherwise = Nothing

main = do
  print $ fib (-1)
  print $ fib 6
