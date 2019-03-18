data Point = Point {x :: Int, y :: Int}
data Rect  = Rect {s_x :: Int, s_y :: Int, w :: Int, h :: Int}

contains :: Rect -> Point -> Bool
contains (Rect {s_x=x, s_y=y, w=w, h=h}) (Point {x=px, y=py})
          = x <= px && px <= x+w && y <= py && py <= y+h


main = do
  print $ contains (Rect 2 2 3 3 ) (Point 1 1)
  print $ contains (Rect 2 2 3 3 ) (Point 2 2)
  print $ contains (Rect 2 2 3 3 ) (Point 3 3)
  print $ contains (Rect 2 2 3 3 ) (Point 4 4)
  print $ contains (Rect 2 2 3 3 ) (Point 5 5)
