import System.Environment
import Data.List

dispatch :: [(String, [String] -> IO ())]
dispatch = [("ls", list)
           ,("add", add)
           ,("remove", remove)
           ]

main = do
        (command:args) <- getArgs
        let (Just action) = lookup command dispatch
        action args


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
