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
