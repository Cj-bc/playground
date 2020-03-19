module Main where
import Data.Hjq
import System.Environment
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as TI

main :: IO ()
main = do
    args <- getArgs

    case args of
        (query:file:[]) -> do
            json <- B.readFile file
            printResult $ hjq json (T.pack query)

        (query:[]) -> do
            json <- B.getContents
            printResult $ hjq json (T.pack query)

        _ -> do putStrLn $ "Invalid arguments error.: " ++ show args
                exitWith $ ExitFailure 1

printResult :: Either T.Text B.ByteString -> IO ()
printResult (Right s) = B.putStrLn s
printResult (Left e) = do
    TI.putStrLn e
    exitWith $ ExitFailure 1
