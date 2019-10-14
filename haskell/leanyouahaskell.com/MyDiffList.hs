newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList l) = l []

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

instance Semigroup (DiffList a) where
    -- prop> (a <> b) <> c == a <> (b <> c)
    --
    --  (DiffList (a ++) <> DiffList (b ++)) <> DiffList (c ++)
    --      = DiffList (\xs -> ((a ++) . (b ++)) xs) <> DiffList (c ++)
    --      = DiffList (\xs -> (a ++ b ++ xs) <> DiffList (c ++)
    --      = DiffList (a ++ b ++) <> DiffList (c ++)
    --      = DiffList (\xs -> (a ++ b ++) . (c ++) xs)
    --      = DiffList (\xs -> (a ++ b ++) (c ++ xs))
    --      = DiffList (\xs -> a ++ b ++ c ++ xs)
    --      = DiffList (a ++ b ++ c ++)
    --
    --  DiffList (a ++) <> (DiffList (b ++) <> DiffList (c ++))
    --      = DiffList (a ++) <> (DffList (\xs -> ((b ++) . (c ++)) xs))
    --      = DiffList (a ++) <> (DffList (\xs -> b ++ c ++ xs))
    --      = DiffList (a ++) <> DffList (b ++ c ++)
    --      = DiffList (\xs -> ((a ++) . (b ++ c ++)) xs)
    --      = DiffList (\xs -> a ++ b ++ c ++ xs)
    --      = DiffList (a ++ b ++ c ++)
    --
    (DiffList a) <> (DiffList b) = DiffList (\xs -> (a . b) xs)

instance Monoid (DiffList a) where
    mempty = DiffList ([] ++)
    mappend = (<>)
