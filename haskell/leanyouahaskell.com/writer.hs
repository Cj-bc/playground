applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = (x' , log `mappend` newLog)
    where
        (x', newLog) = f x
