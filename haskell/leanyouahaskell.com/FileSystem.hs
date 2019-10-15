type Name = String
type Data = String
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

fsUp :: FSZipper -> FSZipper
fsUp (i, FSCrumb name prevItems nextItems:cs)
    = (Directory name (prevItems ++ [i] ++ nextItems), cs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo target (Directory basename items, bs)
    = let (before, i:after) = break (nameIs target) items
      in (i, FSCrumb basename before after:bs)

nameIs :: Name -> FSItem -> Bool
nameIs target (Directory name _) = name == target
nameIs target (File name _) = name == target

fsRename :: Name -> FSZipper -> FSZipper
fsRename n (Directory _ items, bs) = (Directory n items, bs)
fsRename n (File _ c, bs) = (File n c, bs)

(-:) :: a -> (a -> b) -> b
x -: f = f x
