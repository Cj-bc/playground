-- How our program will operate.
-- main = Read list of directories and their sizes
--   Decide how to fit them on CD-Rs
--   Print solution


module Main where

main = do
        input <- getContents
        putStrLn ("DEBUG: get input " ++ input)
