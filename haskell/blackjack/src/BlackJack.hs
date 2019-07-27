{-|
Module: BlackJack
Description: simplest blackjack module.

This module uses rules found in [wikipedia](https://ja.wikipedia.org/wiki/ブラックジャック)
-}

module BlackJack (Card(..), Game, Action, getPoint)
where

import Data.List (sort)
import System.Random

data Card = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K deriving (Eq, Ord)
data Game = Game { player :: [Card]
                 , dealer :: [Card]
                 , deck :: [Card]
                 }

data Action = Hit | Stand


-- | calculate current points from hand
-- Value of A would be one of 1 or 11 depending on other cards
--
-- >>> getPoint [Two, Three]
-- 5
-- >>> getPoint [Two]
-- 2
-- >>> getPoint [J, Q]
-- 20
-- >>> getPoint [Three, Five, A]
-- 19
-- >>> getPoint [J, K, A]
-- 21
getPoint :: [Card] -> Int
getPoint []                  = 0
getPoint cs | A `elem` cs           = if restOfAP <= 10
                                      then 11 + restOfAP
                                      else  1 + restOfAP
            | head cs == Two   = 2  + getPoint (tail cs)
            | head cs == Three = 3  + getPoint (tail cs)
            | head cs == Four  = 4  + getPoint (tail cs)
            | head cs == Five  = 5  + getPoint (tail cs)
            | head cs == Six   = 6  + getPoint (tail cs)
            | head cs == Seven = 7  + getPoint (tail cs)
            | head cs == Eight = 8  + getPoint (tail cs)
            | head cs == Nine  = 9  + getPoint (tail cs)
            | head cs == Ten   = 10 + getPoint (tail cs)
            | head cs == J     = 10 + getPoint (tail cs)
            | head cs == Q     = 10 + getPoint (tail cs)
            | head cs == K     = 10 + getPoint (tail cs)
        where
            restOfAP = getPoint $ tail $ sort cs


-- | Shuffled(randomly picked) deck.
--
-- prop> shuffleDeck g /= shuffleDeck g'
--
-- prop> shuffleDeck g == shuffleDeck g
shuffleDeck :: (RandomGen g) => g -> [Card]
shuffleDeck g = flip shuffle g $ concat $
                  replicate 4 [A, Two, Three, Four, Five, Six, Seven
                              , Eight, Nine, Ten, J, Q, K]



-- | Shuffle list (I hope)
--
-- prop> shuffle xs g /= xs
--
-- prop> shuffle xs g /= shuffle xs g'
--
-- prop> shuffle xs g == shuffle xs g
shuffle :: (RandomGen g) => [a] -> g -> [a]
shuffle x _ | length x == 1 = x
shuffle xs g = picked : shuffle rest g'
  where
    (n, g') = randomR (1, length xs) g
    picked  = xs !! (n - 1)
    rest     = take (n - 1) xs ++ drop n xs
