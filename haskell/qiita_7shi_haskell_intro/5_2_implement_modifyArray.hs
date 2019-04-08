import Data.Array.IO

modifyArray arr idx fn = do
  x <- readArray arr idx
  writeArray arr idx $ fn x

main = do
  a <- newArray (0, 2) 0 :: IO (IOUArray Int Int)
  print =<< getElems a
  modifyArray a 1 (+ 5)
  print =<< getElems a
