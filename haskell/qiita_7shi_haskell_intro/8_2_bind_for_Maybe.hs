bind (Just x) f = f x
bind Nothing _ = Nothing
-- We should use pattern matching in this case.
-- Otherwise, `Nothing` will be also treated in the first line,
-- which will occur an error

main = do
  print $ Just 1 `bind` \a -> Just $ a * 2
  print $ Just 1 `bind` \a -> Nothing `bind` \b -> Just $ a * b
