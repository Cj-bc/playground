module Main where

import TicTacToe

createBoardAA :: Board -> String
createBoardAA (Board b) = let first   = take 3 b
                              second  = drop 3 $ take 6 b
                              third   = drop 6 b
                              toStr x = maybe "-" show x
                          in unlines $ [ concat $ map toStr first
                                       , concat $ map toStr second
                                       , concat $ map toStr third]

runGame :: Mark -> Board -> IO Board
runGame _ b | isFinished b = return b
runGame O b@(Board b')     = do
                             putStrLn $ createBoardAA b
                             putStrLn "where to put (index)"
                             t <- getLine
                             runGame X $ putMark O b (read t :: Int)
runGame X b@(Board b')     = do
                             putStrLn $ createBoardAA b
                             putStrLn "where to put (index)"
                             t <- getLine
                             runGame O $ putMark X b (read t)



main = do
    putStrLn "Tic tac toe:"
    b <- runGame X initializeBoard
    putStrLn $ createBoardAA b
