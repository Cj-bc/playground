applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = (x', log ++ newLog)
    where
        (x', newLog) = f x
