module My.Playground.FileSystem where

import  Data.List.Split (splitOn)

type Name = String
type Data = String
type Path = String
data FSItem = Directory Name [FSItem]
            | File Name Data deriving (Show)

sampleDisk :: FSItem
sampleDisk = Directory "root"
                [ Directory "bin"
                    [ File "bash" "BASH EXECUTABLE"
                    , File "vi" "VI EXECUTABLE"
                    , File "vim" "VIM EXECUTABLE"
                    ]
                , Directory "config"
                    [ File "bashrc" "bashrc CONTENTS"
                    ]
                , Directory "Pic"
                    [ File "Yuaaaa.png" "Datentch Yua's photo"
                    , File "Yozakura_mia_bear.png" "Yozakura Mia(bear)'s photo"
                    ]
                ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> Maybe FSZipper
fsUp (_, []) = Nothing
fsUp (i, FSCrumb name prevItems nextItems:cs)
    = Just (Directory name (prevItems ++ [i] ++ nextItems), cs)


fsTo :: Name -> FSZipper -> Maybe FSZipper
fsTo _ (File _ _, _) = Nothing
fsTo target (Directory basename items, bs)
    | True `elem` map (nameIs target) items
        = let (before, i:after) = break (nameIs target) items
          in Just (i, FSCrumb basename before after:bs)
    | otherwise = Nothing

nameIs :: Name -> FSItem -> Bool
nameIs target (Directory name _) = name == target
nameIs target (File name _) = name == target

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Directory _ items, bs) = (Directory n items, bs)
fsRename n (File _ c, bs) = (File n c, bs)

fsNewFile :: FSItem -> FSZipper -> FSZipper
fsNewFile item (Directory dirname cs, bs) = (Directory dirname (item:cs), bs)

(-:) :: a -> (a -> b) -> b
x -: f = f x


-- | Move to given path
-- This works as sh builtin 'cd' command (I hope)
cd :: Path -> FSZipper -> Maybe FSZipper
cd ('.':'.':ps) z   = fsUp z >>= cd ps
cd ('/':ps) z       = fsUpToRoot z >>= cd ps
    where
        fsUpToRoot :: FSZipper -> Maybe FSZipper
        fsUpToRoot (item, []) = Just (item, [])
        fsUpToRoot z          = fsUp z
cd ps z             = cdTo (splitOn "/" ps) z
    where
        cdTo :: [Path] -> FSZipper -> Maybe FSZipper
        cdTo [] z = Just z
        cdTo ("..":xs) z = fsUp z >>= cdTo xs
        cdTo (x:xs) z = fsTo x z >>= cdTo xs


