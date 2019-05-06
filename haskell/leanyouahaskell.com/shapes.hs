data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shape -> Float
surface (Circle _ l) = pi * l * l
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)




main = do
        let rec = Rectangle (Point 0 0) (Point 10 10)
            cir = Circle (Point 0 0) 10.0
        print $ surface rec
        print $ surface cir
