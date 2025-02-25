module Main where

import System.Random
import Brick
import BlackJack
import BlackJack.Types (AppState(..))
import UI.CUI

main :: IO ()
main = do
  initDeck <- shuffleDeck <$> getStdGen
  let initState = AppState (Game [] [] initDeck DealCard) Nothing
  finalState <- defaultMain app initState
  putStrLn "finishied"
