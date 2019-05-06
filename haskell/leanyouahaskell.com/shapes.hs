module Shapes
(Point(..)
,Shape(..)
,surface
,nudge
) where

data Point = Point Float Float deriving Show
data Shape = Circle Point Float | Rectangle Point Point deriving Show

surface :: Shape -> Float
surface (Circle _ l) = pi * l * l
surface (Rectangle (Point x1 y1) (Point x2 y2)) = abs $ (x2 - x1) * (y2 - y1)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) l)    mx my = Circle (Point (x + mx) (y + my)) l
nudge (Rectangle (Point x y) (Point x2 y2)) mx my = Rectangle (Point (x + mx) (y + my)) (Point (x2 + mx) (y2 + my))
