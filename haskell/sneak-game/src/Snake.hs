module Snake where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|))
import qualified Data.Sequence as S
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((&), (.~), (%~), (^.))
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)



data Game = Game { _snake :: Snake         -- ^ Snake as a sequence of points in R2
                 , _dir   :: Direction     -- ^ Direction
                 , _food  :: Coord         -- ^ Location of the food
                 , _foods :: Stream Coord  -- ^ Infinity list of random foods
                 , _dead  :: Bool          -- ^ Game over flag
                 , _paused :: Bool         -- ^ Paused flag
                 , _score :: Int           -- ^ Score
                 , _frozen :: Bool         -- ^ Freeze to disallow duplicate turns
                 } deriving (Show)

type Coord = V2 Int
type Snake = Seq Coord

data Stream a = a :| Stream a deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

-- | Step forward in time
step :: Game -> Game
step g = fromMaybe g $ do
  guard (not $ g ^. paused || g^.dead)
  let g' = g & frozen .~ False
  return . fromMaybe (move g') $ die g' <|> eatFood g'


-- | Possibly die if next head position is disallowed
die :: Game -> Maybe Game

-- | Possibly eat food if next head position is food
eatFood :: Game -> Maybe Game

-- | Move snake along in a marquee fashion
move :: Game -> Game


-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game


-- | Initialize a paused game with random food location
initGame :: IO Game
