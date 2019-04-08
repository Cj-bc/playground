import System.Random

shuffle [] = return []
shuffle xs =
  getStdRandom (randomR (0, length xs - 1)) >>= \n ->
  shuffle (take n xs ++ drop (n + 1) xs) >>= \xs' ->
  return $ (xs !! n) : xs'

main :: IO ()
main = do
  xs <- shuffle [1..9]
  print xs
