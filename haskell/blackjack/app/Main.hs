module Main where

import System.Random
import BlackJack
import BlackJack.Types (AppState(..))

main :: IO ()
main = do
  putStrLn "===Black Jack==="
  initDeck <- shuffleDeck <$> getStdGen

  (result, winner) <- runGame (Game [] [] initDeck DealCard)
  putStrLn $ "Winner: " <> (show winner)


runGame :: Game -> IO (Game, Player)
runGame g@(Game _ _ _ (GameEnd winner)) = return (g, winner)
runGame g@(Game _ _ _ DealCard) = do
                                    g' <- doPhase $ AppState g askAction
                                    putStrLn $ "Your hand: " ++ show (player g')
                                        ++ " | dealer's hand: ["
                                        ++ show (head (dealer g')) ++ " * ]"
                                    runGame g'
runGame g@(Game _ _ _ PlayerTurn) = do
                                      g' <- doPhase $ AppState g askAction
                                      putStrLn $ "Your hand: " ++ show (player g')
                                          ++ " | dealer's hand: ["
                                          ++ show (head (dealer g')) ++ " * ]"
                                      runGame g'
runGame g@(Game _ _ _ DealerTurn) = do
                                      g' <- doPhase $ AppState g askAction
                                      putStrLn $ "Your hand: " ++ show (player g')
                                          ++ " | dealer's hand: "
                                          ++ show (dealer g')
                                      runGame g'
runGame g = doPhase (AppState g askAction) >>= runGame


askAction :: IO Action
askAction = do
              putStr "hit? stand?('hit'/'stand')\n> "
              ans <- getLine
              case ans of
                "hit" -> return Hit
                "stand" -> return Stand
