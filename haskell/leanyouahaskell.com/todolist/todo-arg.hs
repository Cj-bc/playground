import System.Environment
import System.IO
import Data.List
import System.Directory

dispatch :: [(String, [String] -> IO ())]
dispatch = [("ls", list)
           ,("add", add)
           ,("remove", remove)
           ]

main = do
        (command:args) <- getArgs
        let action_ = lookup command dispatch
        case action_ of
                Just action -> action args
                Nothing -> return ()


list :: [String] -> IO ()
list [file] = do
        contents <- readFile file
        putStrLn contents

add :: [String] -> IO ()
add [file, newtitle] = appendFile file (newtitle ++ "\n")


remove :: [String] -> IO ()
remove [file, numStr] = do
        contents <- readFile file
        let prevTodo              = lines contents
            num                   = read numStr
            newTodo               = delete (prevTodo !! num) prevTodo
            (tmpName, tmpHandler) = openTempFile "." "tmp"
        writeFile tmpName $ unlines newTodo
        hClose tmpHandler
        removeFile file
        renameFile tmpName file
