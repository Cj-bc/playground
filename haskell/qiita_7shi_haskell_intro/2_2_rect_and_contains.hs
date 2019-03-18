data Point = Point Int Int
data Rect  = Rect Int Int Int Int

contains :: Rect -> Point -> Bool
contains (Rect _ _ _ 0) _ = False
contains (Rect _ _ 0 _) _ = False
contains (Rect x y w h) (Point px py)
          | x   > px  = False
          | y   > py  = False
          | x+w < px  = False
          | y+h < py  = False
          | otherwise = True

main = do
  print $ contains (Rect 2 2 3 3 ) (Point 1 1)
  print $ contains (Rect 2 2 3 3 ) (Point 2 2)
  print $ contains (Rect 2 2 3 3 ) (Point 3 3)
  print $ contains (Rect 2 2 3 3 ) (Point 4 4)
  print $ contains (Rect 2 2 3 3 ) (Point 5 5)
