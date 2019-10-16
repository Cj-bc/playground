module My.Playground.ListZipper where

type ListZipper a =([a], [a])

goFoward :: ListZipper a -> ListZipper a
goFoward (x:xs,bs) = (xs, x:bs)

goBack :: ListZipper a -> ListZipper a
goBack (xs, b:bs) = (b:xs, bs)
