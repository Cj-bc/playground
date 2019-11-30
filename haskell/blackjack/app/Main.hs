module Main where

import System.Random
import BlackJack

main :: IO ()
main = do
  putStrLn "===Black Jack==="
  initDeck <- shuffleDeck <$> getStdGen

  (result, winner) <- runGame (Game [] [] initDeck DealCard)
  putStrLn $ "Winner: " <> (show winner)


runGame :: Game -> IO (Game, Player)
runGame g@(Game _ _ _ (GameEnd winner)) = return (g, winner)
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
