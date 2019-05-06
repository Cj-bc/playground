module TrafficLight
( TrafficLight(..)
) where

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
        Red == Red = True
        Yellow == Yellow = True
        Green == Green = True
        _ == _ = False

instance Show TrafficLight where
        show Red = "Red light"
        show Yellow = "Yellow light"
        show Green = "Green light"
