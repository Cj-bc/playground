import System.IO
import System.Environment
import Control.Exception

main = tryDo `catch` handler

tryDo :: IO ()
tryDo = do
        (fileName:_) <- getArgs
        contents <- readFile fileName
        putStrLn $ "File '" ++ fileName ++ "' has " ++ show (length (lines contents)) ++ "lines"

handler :: IOError -> IO ()
handler e = putStrLn "Exception occured"
