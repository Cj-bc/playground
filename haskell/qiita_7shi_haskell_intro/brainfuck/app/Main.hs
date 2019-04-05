module Main where

import Data.Array.IO
import Data.Char
import Data.IORef
import Debug.Trace

data Order = Plus | Minus | Rshift | Lshift | Print | Input | Lstart | Lend deriving Eq

convertOrder :: Char -> Order
convertOrder '+' = Plus
convertOrder '-' = Minus
convertOrder '>' = Rshift
convertOrder '<' = Lshift
convertOrder '.' = Print
convertOrder ',' = Input
convertOrder '[' = Lstart
convertOrder ']' = Lend


-- Returns list of jmpBackList {{{
-- example input:
--    jmpBackList "--[+[-.,.]<+->]" []
-- expected return
--    [0, 0, 15, 0, 10, 0, 0, 0, 0, 5, 0, 0, 0, 0, 3]
--
--    jmpBackList "--[+[-.,.]<+->]" []
--    [0] ++ jmpBackList "-[+[-.,.]<+->]" [-1]
--    [0] ++ [0] ++ jmpBackList "[+[-.,.]<+->]" [-1, -1]
--    [0] ++ [0] ++ [0] ++ jmpBackList "+[-.,.]<+->]" [-1, -1, 3]
--    [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList "[-.,.]<+->]" [-1, -1, 3, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList "-.,.]<+->]" [-1, -1, 3, -1, 5]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList ".,.]<+->]" [-1, -1, 3, -1, 5, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList ",.]<+->]" [-1, -1, 3, -1, 5, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList ".]<+->]" [-1, -1, 3, -1, 5, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList "]<+->]" [-1, -1, 3, -1, 5, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ jmpBackList "<+->]" [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ jmpBackList "+->]" [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ jmpBackList "->]" [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ jmpBackList ">]" [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ jmpBackList "]" [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3] ++ jmpBackList [] [-1, -1, 3, -1, -1, -1, -1, -1, -1, -1]
--    [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3] ++ []
--    [0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 3]
--    }}}
jmpBackList :: [Char] -> [Integer] -> [Integer]
jmpBackList [] _                  = []
jmpBackList xs []                 = jmpBackList xs [-1]
jmpBackList (x:xs) lp | x == '['  = jmpBackList xs $ lp ++ [toInteger (length lp) - 1]
                      | x == ']'  = [s] ++ jmpBackList xs lp'
                      | otherwise = [0] ++ jmpBackList xs (lp ++ [-1])
                          where
                              s   = last [n | n <- lp, n /= -1]
                              lp' = [n | n <- lp, n /= s] ++ [-1]


-- trace {{{
-- jmpFrontList [0, 0, 0, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0, 3]
-- jmpFrontList [0, 0, 15, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0, 0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 0, 0, 0, 0, 0, 5, 0, 0, 0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 0, 0, 0, 0, 0, 5, 0, 0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 0, 0, 0, 0, 0, 5, 0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 0, 0, 0, 0, 0, 5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 10, 0, 0, 0, 0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 10, 0, 0, 0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 10, 0, 0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 10, 0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0, 10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15, 0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0, 15] ++ [0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0, 0] ++ [15] ++ [0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [0] ++ [0] ++ [15] ++ [0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- jmpFrontList [] ++ [0] ++ [0] ++ [15] ++ [0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- [] ++ [0] ++ [0] ++ [15] ++ [0] ++ [10] ++ [0] ++ [0] ++ [0] ++ [0] ++ [5] ++ [0] ++ [0] ++ [0] ++ [0] ++ [3]
-- [0, 0, 15, 0, 10, 0, 0, 0, 0, 5, 0, 0, 0, 0, 3]
-- }}}
jmpFrontList :: [Integer] -> [Integer]
jmpFrontList [] = []
jmpFrontList xs | last xs == 0                      = (jmpFrontList xs') ++ [0]
                | last xs >= toInteger (length xs)  = (jmpFrontList xs') ++ [last xs]
                | otherwise                         = (jmpFrontList modified) ++ [last xs]
                where
                    xs' = init xs
                    modified = take foward xs' ++ [toInteger (length xs - 1) ] ++ drop (foward + 1) xs'
                    foward   = fromIntegral $ last xs :: Int


main = do
    let bf = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+."
--    let bf = ">+++++++++[<++++++++>-]<.>+++++++[<++++>" ++
--             "-]<+.+++++++..+++.[-]>++++++++[<++++>-]<" ++
--             ".>+++++++++++[<+++++>-]<.>++++++++[<+++>" ++
--             "-]<.+++.------.--------.[-]>++++++++[<++" ++
--             "++>-]<+.[-]++++++++++."

    let jump = jmpFrontList $ jmpBackList bf []
    print jump
    mem     <- newArray (0, 30000) 0 :: IO (IOUArray Int Int)
    ptr     <- newIORef 0 -- when I change this to '1', it seems to be go better
    current <- newIORef 0

    let loop = do
                c_ptr     <- readIORef ptr
                c_current <- readIORef current
                c_mem     <- readArray mem c_ptr
                writeIORef current $ c_current + 1
                case convertOrder (bf !! c_current) of
                    Plus    -> writeArray mem c_ptr $ c_mem + 1
                    Minus   -> writeArray mem c_ptr $ c_mem - 1
                    Rshift  ->  (writeIORef ptr $ c_ptr + 1)
                    Lshift  ->  (writeIORef ptr $ c_ptr - 1)
                    Print   -> putChar $ chr c_mem
--                  Input   -> # not implemented yet
                    Lstart | c_mem == 0 -> writeIORef current $ fromIntegral $ jump !! c_current
                           | otherwise  -> return ()
                    Lend   | c_mem /= 0 -> trace ("Lend to " ++ (show (jump !! (c_current - 1))) ++ ", " ++ (show c_current)) writeIORef current $ fromIntegral $ jump !! (c_current - 1)
                           | otherwise  -> return ()
                    _       -> putStrLn $ "error: " ++ [bf !! c_current]
                if c_current < (length bf - 1) then loop else return ()
    loop
    putStrLn "END"
