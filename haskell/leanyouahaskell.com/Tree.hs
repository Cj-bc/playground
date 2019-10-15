module Tree
( Tree(..)
, singleton
, treeInsert
, treeElem
, numTree
) where

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance Functor Tree where
        fmap f EmptyTree = EmptyTree
        fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

singleton :: (Ord a) => a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
        | x == a = Node a left right
        | x <  a = Node a (treeInsert x left) right
        | x >  a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
        | x == a = True
        | x <  a = treeElem x left
        | x >  a = treeElem x right


numTree :: Tree Int
numTree = foldr treeInsert EmptyTree [1,4,2,9,8]

freeTree :: Tree Char
freeTree = Node 'P'
                (Node 'O'
                      (Node 'L'
                            (Node 'N' EmptyTree EmptyTree)
                            (Node 'T' EmptyTree EmptyTree)
                      )
                      (Node 'Y'
                            (Node 'S' EmptyTree EmptyTree)
                            (Node 'A' EmptyTree EmptyTree)
                      )
                )
                (Node 'L'
                      (Node 'W'
                            (Node 'C' EmptyTree EmptyTree)
                            (Node 'R' EmptyTree EmptyTree)
                      )
                      (Node 'A'
                            (Node 'A' EmptyTree EmptyTree)
                            (Node 'C' EmptyTree EmptyTree)
                      )
                )

data Direction = R | L deriving (Show)
type Directions = [Direction]

changeToP :: Directions -> Tree String -> Tree String
changeToP [] (Node _ l r) = Node "P" l r
changeToP (L:xs) (Node a l r) = Node a (changeToP xs l) r
changeToP (R:xs) (Node a l r) = Node a l (changeToP xs r)

elemAt :: Directions -> Tree a -> a
elemAt [] (Node a l r)  = a
elemAt (L:xs) (Node _ l _) = elemAt xs l
elemAt (R:xs) (Node _ _ r) = elemAt xs r

type Breadcrumbs = Directions

goLeft :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goLeft (Node _ l _, bs) = (l, L:bs)

goRight :: (Tree a, Breadcrumbs) -> (Tree a, Breadcrumbs)
goRight (Node _ _ r, bs) = (r, R:bs)

a -: f = f a
