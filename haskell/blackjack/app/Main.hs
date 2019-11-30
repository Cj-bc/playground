module Main where

import System.Random
import BlackJack
import BlackJack.Types (AppState(..))
import UI.CLI (askAction, runGame)

main :: IO ()
main = do
  putStrLn "===Black Jack==="
  initDeck <- shuffleDeck <$> getStdGen

  (result, winner) <- runGame (Game [] [] initDeck DealCard)
  putStrLn $ "Winner: " <> (show winner)
