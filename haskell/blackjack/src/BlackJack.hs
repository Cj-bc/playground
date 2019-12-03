{-|
Module: BlackJack
Description: simplest blackjack module.

This module uses rules found in [wikipedia](https://ja.wikipedia.org/wiki/ブラックジャック)
-}

module BlackJack (Card(..), Player(..), Phase(..), Action(..), Game(..), doPhase, doAction, getPoint, shuffleDeck)
where

import Data.List (sort)
import Data.Maybe (fromMaybe, fromJust)
import System.Random
import BlackJack.Types



-- | Cycle Phase Cycle
-- If you call this again and again, it'll continue game automatically.
--
-- >>> doPhase $ AppState (Game [] [] [A, Two, Three, Four, Five, Six] DealCard) (\_ -> return Hit)
-- Game [A, Two] [Three, Four] [Five, Six] PlayerTurn
-- >>> doPhase $ AppState (Game [A, Two] [] [Three, Four] PlayerTurn) (\_ -> return Hit)
-- Game [A, Two, Three] [] [Four] PlayerTurn
-- >>> doPhase $ AppState (Game [A, Two] [] [Three, Four] PlayerTurn) (\_ -> return Stand)
-- Game [A, Two] [] [Three, Four] PlayerTurn
--
-- >>> doPhase $ AppState (Game [] [Three, Four] [Five, Six] DealerTurn) (\_ -> return Hit)
-- Game [] [Three,Four,Five] [Six] DealerTurn
-- >>> doPhase $ AppState (Game [] [Seven, Ten] [Five, Six] DealerTurn) (\_ -> return Hit)
-- Game [] [Seven,Ten] [Five,Six] ComparePoints
-- >>> doPhase $ AppState (Game [] [Seven, Ten] [] DealerTurn) (\_ -> return Hit)
-- Game [] [Seven,Ten] [Five,Six] ComparePoints-
-- >>> doPhase $ AppState (Game [A, Ten] [Two, Three] [] ComparePoints) (\_ -> return Hit)
-- Game [A,Ten] [Two,Three] [] (GameEnd Player)
-- >>> doPhase $ AppState (Game [Two, Three], [A, Ten] [] ComparePoints) (\_ -> return Hit)
-- Game [Two,Three] [A,Ten] [] (GameEnd Dealer)
doPhase :: AppState -> Game
doPhase (AppState g action) | phase g == DealCard =
                                    let g' =  g { player = take 2 (deck g)
                                                , dealer = drop 2 $ take 4 (deck g)
                                                , deck   = drop 4 (deck g)
                                                , phase  = PlayerTurn }
                                        Just checked = doAction g' BustCheck
                                    in checked
                               | phase g == PlayerTurn = case doAction g (fromJust action) of
                                                            Nothing -> (g {phase = DealerTurn})
                                                            Just g' -> (fromMaybe g' (doAction g' BustCheck))
                               | phase g == DealerTurn =
                                    let newGame = if getPoint (dealer g) < 17
                                                  then fromMaybe (g {phase = ComparePoints}) $ doAction g Hit
                                                  else fromJust (doAction g Stand)
                                    in newGame
                               | phase g == ComparePoints =
                                    let Just g' = doAction g BustCheck
                                        hasMorePoint = if getPoint (player g) > getPoint (dealer g)
                                                       then Player
                                                       else Dealer
                                    in if (phase g' == GameEnd Player || phase g' == GameEnd Dealer)
                                        then g'
                                        else g { phase = GameEnd hasMorePoint}
                              | phase g == GameEnd Player ||
                                phase g == GameEnd Dealer =  g


-- | Change game state based on current Game and occurred Action
-- Return Nothing if action failed.
--
-- >>> doAction (Game [A, Two] [Three, Four] [] PlayerTurn) Hit
-- Nothing
-- >>> doAction (Game [A, Two] [Three, Four] [J, K, Q] PlayerTurn) Hit
-- Just (Game {player = [A,Two,J], dealer = [Three,Four], deck = [K,Q], phase = PlayerTurn})
-- >>> doAction (Game [A, Two] [Three, Four] [J, K, Q] DealerTurn) Hit
-- Just (Game {player = [A,Two], dealer = [Three,Four,J], deck = [K,Q], phase = DealerTurn})
-- >>> doAction (Game [A, Two] [Three, Four] [J, K, Q] PlayerTurn) Stand
-- Just (Game {player = [A,Two], dealer = [Three,Four], deck = [J,K,Q], phase = DealerTurn})
-- >>> doAction (Game [A, Two] [Three, Four] [J, K, Q] DealerTurn) Stand
-- Just (Game {player = [A,Two], dealer = [Three,Four], deck = [J,K,Q], phase = ComparePoints})
doAction :: Game -> Action -> Maybe Game
doAction g@(Game _ _ [] _) Hit    = Nothing
doAction g@(Game p _ deck PlayerTurn) Hit = Just g {player = p ++ [head deck],
                                                    deck = tail deck}
doAction g@(Game _ d deck DealerTurn) Hit = Just g {dealer = d ++ [head deck],
                                                    deck = tail deck}
doAction g@(Game _ _ _ PlayerTurn) Stand  = Just g {phase = DealerTurn}
doAction g@(Game _ _ _ DealerTurn) Stand  = Just g {phase = ComparePoints}
doAction g@(Game p d _ _) BustCheck | getPoint p > 21 = Just g {phase = GameEnd Dealer}
                                    | getPoint d > 21 = Just g {phase = GameEnd Player}
                                    | otherwise       = Just g

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
getPoint []                    = 0
getPoint cs | A `elem` cs      = if restOfAP <= 10
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
