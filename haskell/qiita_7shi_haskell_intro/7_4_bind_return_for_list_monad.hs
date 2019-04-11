bind xs fn = foldr fn [] xs

return' x = [x]

main = do
--  print $ [1..3] `bind` \x -> "abc" `bind` \y -> return' (x, y)
  print $ bind [1..3] (\x -> bind "abc" (\y -> return' (x, y)))
