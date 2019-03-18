mkTupleOfEdges = [(x, y, z)| x <- [1..20], y <- [1..20], z <- [1..20]]

isTriangle :: (Int, Int, Int) -> Bool
isTriangle (x, y, z) | x^2 + y^2 == z^2 = True
                     | x^2 + z^2 == y^2 = True
                     | y^2 + z^2 == x^2 = True
                     | otherwise        = False



main = do
  -- The First one is my solution,
  -- The second one is example solution.
  putStrLn $ "my solution:" ++ show [ t | t <- mkTupleOfEdges, isTriangle t]
  putStrLn $ "sample solution:" ++ show [ (x, y, z) | x <- [1..20], y <- [1..20], z <- [1..20],
                                x^2 + y^2 == z^2 ||
                                x^2 + z^2 == y^2 ||
                                y^2 + z^2 == x^2 ]
