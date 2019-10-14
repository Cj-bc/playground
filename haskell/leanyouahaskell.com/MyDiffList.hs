newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList l) = l []

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

instance Semigroup (DiffList a) where
    (DiffList a) <> (DiffList b) = let in_a = a []
                                       in_b = b []
                                   in DiffList (\xs -> in_a ++ in_b ++ xs)

instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)
    mappend = (<>)
