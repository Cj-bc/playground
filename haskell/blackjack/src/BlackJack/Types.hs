module BlackJack.Types where


data Card = A | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K deriving (Eq, Ord, Show)
data Player = Player | Dealer deriving (Show, Eq)
data Phase = DealCard | PlayerTurn | DealerTurn | ComparePoints | GameEnd Player deriving (Show, Eq)
data Game = Game { player :: [Card] -- ^ Player's hand
                 , dealer :: [Card] -- ^ Dealer's hand
                 , deck :: [Card]   -- ^ Cards of deck
                 , phase :: Phase   -- ^ Describe what to do next
                 } deriving (Show)

data AppState = AppState { app_game :: Game
                         , app_askAction :: IO Action }

data Action = Hit | Stand | BustCheck
-- ^ All actions that should be happened in game.
