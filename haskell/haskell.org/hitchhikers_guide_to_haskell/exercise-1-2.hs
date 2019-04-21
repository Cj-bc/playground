module Main where

main = do
        putStr "What's your name: "
        name <- getLine
        putStrLn $ "Welcome " ++ name ++ " !"
        putStr "What's your favorite color: "
        color <- getLine
        putStrLn $ "Color: " ++ color
