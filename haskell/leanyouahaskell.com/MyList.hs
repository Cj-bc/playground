module My.Playground.MyList
( List(..)
) where

infix 5 :-:
data List a = EmptyList | a :-: (List a) deriving (Show, Read, Eq, Ord)
