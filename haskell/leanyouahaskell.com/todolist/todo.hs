import Data.Char
import System.IO
import System.Directory
import Data.List
import Control.Monad

main = do
        putStrLn $ unlines ["=============== TODO LIST ==============="
                           ,"1) List your TODO from \"todo.txt\""
                           ,"2) Add todo"
                           ,"3) Remove todo"
                           ,"4) Quit"]
        putStr "Your choice: "
        choice <- getLine
        case choice of
                "1" -> do
                  listTodo
                  main
                "2" -> do
                  title <- prompt "Title: "
                  addTodo title
                  main
                "3" -> do
                  num <- prompt "Which to remove: "
                  removeTodo (read num :: Int)
                  main
                "4" -> do
                  return ()
                _   -> return ()


prompt :: String -> IO String
prompt message = do
        putStr message
        answer <- getLine
        return answer


addTodo :: String -> IO ()
addTodo title = appendFile "todo.txt" $ title ++ "\n"

removeTodo :: Int -> IO ()
removeTodo number = do
        contents <- readFile "todo.txt"
        (tmpName, tmpHandler) <- openTempFile "." "tmp"
        let todoTasks   = lines contents
            newcontents = delete (todoTasks !! number) todoTasks
        hPutStr tmpHandler $ unlines newcontents
        hClose tmpHandler
        removeFile "todo.txt"
        renameFile tmpName "todo.txt"

listTodo :: IO ()
listTodo = do
        contents <- readFile "todo.txt"
        putStr contents
