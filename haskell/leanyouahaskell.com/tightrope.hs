type Birds = Int
type Pole = (Birds, Birds)

(-:) :: a -> (a -> b) -> b
x -: f = f x

landLeft :: Birds -> Pole -> Pole
landLeft bs (l, r) = (l + bs, r)

landRight :: Birds -> Pole -> Pole
landRight bs (l, r) = (l, r + bs)

