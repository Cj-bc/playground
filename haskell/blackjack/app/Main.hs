module Main where

import System.Random
import BlackJack

main :: IO ()
main = do
  putStrLn "===Black Jack==="
  initDeck <- shuffleDeck <$> getStdGen

  result <- runGame (Game [] [] initDeck DealCard)
  case phase result of
    GameEnd Player -> putStrLn "winner: player"
    GameEnd Dealer -> putStrLn "winner: dealer"
    otherwise -> putStrLn "an unknown error ocured"


runGame :: Game -> IO Game
runGame g@(Game _ _ _ (GameEnd winner)) = return g
runGame g@(Game _ _ _ DealCard) = do
                                    g' <- doPhase g
                                    putStrLn $ "Your hand: " ++ show (player g')
                                        ++ " | dealer's hand: ["
                                        ++ show (head (dealer g')) ++ " * ]"
                                    runGame g'
runGame g@(Game _ _ _ PlayerTurn) = do
                                      g' <- doPhase g
                                      putStrLn $ "Your hand: " ++ show (player g')
                                          ++ " | dealer's hand: ["
                                          ++ show (head (dealer g')) ++ " * ]"
                                      runGame g'
runGame g@(Game _ _ _ DealerTurn) = do
                                      g' <- doPhase g
                                      putStrLn $ "Your hand: " ++ show (player g')
                                          ++ " | dealer's hand: "
                                          ++ show (dealer g')
                                      runGame g'
runGame g = doPhase g >>= runGame
