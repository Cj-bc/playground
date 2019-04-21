-- How our program will operate.
-- main = Read list of directories and their sizes
--   Decide how to fit them on CD-Rs
--   Print solution


module Main where
import Text.ParserCombinators.Parsec

-- parseInput parses output of "du -sb", which consists of many lines.
-- each of which describes single directory
parseInput = do
        dirs <- many dirAndSize
        eof :: Parser ()
        return dirs

-- Datatype Dir holds information about single directory - its size and name
data Dir = Dir Int String deriving Show

-- `dirAndSize` parses information about single directory, which is:
-- a size in bytes (number), some spaces then directory name, which extends till newline
dirAndSize = do
        size <- many1 digit
        spaces
        dir_name <- anyChar `manyTill` newline
        return (Dir (read size) dir_name)


main = do
        input <- getContents
        putStrLn ("DEBUG: get input " ++ input)
