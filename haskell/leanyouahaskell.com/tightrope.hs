module My.Playground.Tightrope where

type Birds = Int
type Pole = (Birds, Birds)

(-:) :: a -> (a -> b) -> b
x -: f = f x

landLeft :: Birds -> Pole -> Maybe Pole
landLeft bs (l, r) | abs ((l + bs) - r) < 4 = Just (l + bs, r)
                   | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight bs (l, r) | abs ((r + bs) - l) < 4 = Just (l, r + bs)
                    | otherwise = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing
