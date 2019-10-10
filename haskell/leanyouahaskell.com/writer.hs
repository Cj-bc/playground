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
