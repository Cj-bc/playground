import Control.Applicative

-- mine {{{
-- mapMaybe :: (a -> Maybe b) -> [a] -> [b]
-- mapMaybe _ [] = []
-- mapMaybe f (x:xs) = filter (/= Nothing) $ foldr ((++) . f) [] xs
-- }}}
mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe f = (`foldr` []) $ \x xs -> case f x of
  Just x' -> x' : xs
  Nothing -> xs
-- (`foldr` [])
--    |  As foldr is used like 'infix function',
--    | '[]' is 2nd argument of 'foldr'.
--    |  The first argument is actually missing
--    | so that it'll take it from backward.
--
-- (`foldr` []) $ \x xs -> ...
--    |  '$ \x xs ...' is the first argument of
--    | 'foldr'.
--
-- The last argument is supplied as 


fact 0 = Just 1
fact n  | n > 0     = (n *) <$> fact (n - 1)
        | otherwise = Nothing

facts n = ( map       fact [n, n - 1, n - 2],
            mapMaybe  fact [n, n - 1, n - 2])

main = do
  print $ facts 3
  print $ facts 2
  print $ facts 1
  print $ facts 0
