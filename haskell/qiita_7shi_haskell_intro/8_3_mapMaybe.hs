import Control.Applicative

mapMaybe' :: (a -> Maybe b) -> [a] -> [b]
mapMaybe' f []     = []
mapMaybe' f (x:xs) = ret : mapMaybe' f xs
  where
    x'          = f x
    (Just ret)  = if x' == Nothing then Just [] else x'


fact 0 = Just 1
fact n  | n > 0     = (n *) <$> fact (n - 1)
        | otherwise = Nothing

facts n = ( map       fact [n, n - 1. n - 2],
            mapMaybe'  fact [n, n - 1. n - 2])

main = do
  print $ facts 3
  print $ facts 2
  print $ facts 1
  print $ facts 0
