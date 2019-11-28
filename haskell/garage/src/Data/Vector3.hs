module Data.Vector3 where

data Vector3 = Vector3 Float Float Float


instance Num Vector3 where
    (Vector3 a1 b1 c1) + (Vector3 a2 b2 c2) = Vector3 (a1 + a2) (b1 + b2) (c1 + c2)
    (Vector3 a1 b1 c1) - (Vector3 a2 b2 c2) = Vector3 (a1 - a2) (b1 - b2) (c1 - c2)
    abs (Vector3 a b c) = Vector3 (abs a) (abs b) (abs c)

magnitude :: Vector3 -> Float
magnitude (Vector3 x y z) = sqrt $ x^2 + y^2 + z^2

distance :: Vector3 -> Vector3 -> Float
distance a b
    = magnitude $ abs (a - b)


