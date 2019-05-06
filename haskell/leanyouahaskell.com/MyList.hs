module MyList
( List(..)
) where

data List a = EmptyList | Cons a (List a) deriving (Show, Read, Eq, Ord)
