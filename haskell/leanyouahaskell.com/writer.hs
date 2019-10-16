module My.Playground.Writer where

import Data.Monoid
import Control.Monad.Writer

applyLog :: Monoid m => (a, m) -> (a -> (b, m)) -> (b, m)
applyLog (x, log) f = (x' , log `mappend` newLog)
    where
        (x', newLog) = f x

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "Sushi"     = ("Green Tea", Sum 120)
addDrink "Hamburger" = ("Cola", Sum 100)
addDrink "Bread"     = ("Cocoa", Sum 100)
addDrink _           = ("Water", Sum 10)


gcd' :: Int -> Int -> Int
gcd' x y | y == 0 = x
         | otherwise = gcd' y (x `mod` y)

gcdWriter :: Int -> Int -> Writer [String] Int
gcdWriter a b | b == 0 = return a
              | otherwise = do
                  tell [show a ++ " div " ++ show b]
                  gcdWriter b (a `mod` b)
