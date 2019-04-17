import Data.Char
import Control.Monad
import Control.Applicative

numUpper :: Int -> Int -> String -> Maybe String
numUpper 0 0 s = Just s
numUpper 0 y (s:ss) = do
  let next    = 1 + ord s
      target' = ord (head ss)
  guard $ next == target'
  guard $ ss /= []
  (++) [s] <$> numUpper 0 (y - 1) ss
numUpper x y (s:ss) = do
  let next    = 1 + ord s
      target' = ord (head ss)
  guard $ next == target'
  guard $ ss /= []
  (++) [s] <$> numUpper (x - 1) y ss

main :: IO ()
main = do
  print $ numUpper 3 2 "123AB"
  print $ numUpper 3 2 "123ABC"
  print $ numUpper 3 2 "12ABC"
