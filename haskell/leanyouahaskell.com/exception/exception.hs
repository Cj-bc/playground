import System.IO
import System.IO.Error
import System.Environment
import Control.Exception

main = tryDo `catch` handler

tryDo :: IO ()
tryDo = do
        (fileName:_) <- getArgs
        contents <- readFile fileName
        putStrLn $ "File '" ++ fileName ++ "' has " ++ show (length (lines contents)) ++ "lines"

handler :: IOError -> IO ()
handler e | isDoesNotExistError e = putStrLn "File does not exist"
          | otherwise             = ioError e
