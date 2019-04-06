module Main where
import System.IO.Unsafe

trace' :: String -> a -> a
trace' str ret = unsafePerformIO $ do
    putStrLn str
    return ret

fact 0 = trace' "fact 0 = 1" 1
fact n | n > 0 = trace' dbg0 $ trace' dbg1 ret
    where
        ret = n * fn1
        fn1 = fact $ n - 1
        dbg0 = "fact " ++ show n ++ " = " ++ show n ++ " * fact " ++ show (n - 1)
        dbg1 = dbg0 ++ " = " ++ show n ++ " * " ++ show fn1 ++ " = " ++ show ret

main :: IO ()
main = do
    print $ fact 5
