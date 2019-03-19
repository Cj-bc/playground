module Main where
import System.Random

shuffle :: [Int] -> IO [Int]
shuffle x | length x == 1 = return x
          | otherwise     = do
            r <- getStdRandom $ randomR (0, length x -1)
            r' <- shuffle [n |n<- x, n /= x !! r]
            return $ [x !!r] ++ r'

main = do
  print =<< shuffle [1..9]
