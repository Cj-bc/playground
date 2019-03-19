module Main where

import System.Random

shuffle :: Eq a => [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  r <- getStdRandom $ randomR (0, length xs -1)
  r' <- shuffle [n| n <- xs, n /= xs!!r]
  return $ [xs!!r] ++ r'


isSorted :: Ord a => [a] -> Bool
isSorted x | length x == 1 = True
isSorted (x:xs) | x < head xs && isSorted xs  = True
                | otherwise                   = False

bogosort :: Ord a => [a] -> IO [a]
bogosort xs | isSorted xs = return xs
            | otherwise   = bogosort =<< shuffle xs

main = do
  xs <- shuffle [1..9]
  print xs
  print =<< bogosort xs
