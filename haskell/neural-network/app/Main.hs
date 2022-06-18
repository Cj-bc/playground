module Main where

import Neuralnet
import Control.Monad (liftM, void)

loss :: [Float] -> [Float] -> Float
loss corrects results = (sum $ zipWith (\c r -> (c - r) ** 2) corrects results) / 2

main :: IO ()
main = do
  sequence $ do
    x <- [1, 0]
    y <- [1, 0]
    pure $ print (x, y) >> print (nandN x y)

  let traindata = concat $ replicate 5 [((1, 0), 1.0), ((0, 1), 1.0), ((1, 1), 0.0), ((0, 0), 0.0)]
      (w11, w12, b1, w21, w22, b2, w31, w32, b3) = (-0.3, -0.3, 0.5, 0.7, 0.7, -0.3, -0.3, -0.3, 0.5)
      result = xorNet w11 w12 b1 w21 w22 b2 w31 w32 b3 <$> (fmap (fst . fst) traindata) <*> (fmap (snd . fst) traindata)
      
      cost = loss (snd <$> traindata) result

  print cost


-- | Network for xor. All parameters should be given by user
xorNet :: Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
xorNet w11 w12 b1 w21 w22 b2 w31 w32 b3 i1 i2 =
  let s1 = perceptron' w11 w12 b1 i1 i2
      s2 = perceptron' w21 w22 b2 i1 i2
      s3 = perceptron w31 w32 b3 id s1 s2
  in s3
