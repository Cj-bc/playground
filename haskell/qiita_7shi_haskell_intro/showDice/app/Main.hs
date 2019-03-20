module Main where
import System.Random

showDice :: IO Int
showDice = do
  r <- getStdRandom $ randomR(0, 9)
  print r
  return r

main = do
  showDice
  showDice
  print =<< showDice
