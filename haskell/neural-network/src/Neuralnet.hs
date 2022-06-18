module Neuralnet where

import Data.Bool (bool)

-- | Simple perceptron implementation
perceptron :: Float -> Float -> Float -> (Float -> Float) -> Float -> Float -> Float
perceptron w1 w2 bias activateFunc x1 x2 = activateFunc $ w1*x1 + w2*x2 + bias
    
perceptron' :: Float -> Float -> Float -> Float -> Float -> Float
perceptron' w1 w2 bias x1 x2 = perceptron w1 w2 bias (bool 1 0 . (>=) 0) x1 x2

andN :: Float -> Float -> Float
andN = perceptron' 0.5 0.5 (-0.7)

orN :: Float -> Float -> Float
orN = perceptron' 0.7 0.7 (-0.3)

nandN :: Float -> Float -> Float
nandN = perceptron' (-0.3) (-0.3) 0.5
