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
-- die g =

-- | Possibly eat food if next head position is food
-- TODO: Should generate next food
eatFood :: Game -> Maybe Game
eatFood g = if nextCoord g^.dir g^.coord == g^.food
              then Just setNextFood . setNewFood . setNewSnake g
              else Nothing
            where
              (head, tail)  = g^.foods
              setNextFood g = g&food~.head
              setNewFoods g = g&foods~.tail
              setNewSnake g = g&snake |> g^.coord

-- | Move snake along in a marquee fashion
move :: Game -> Game
move g =
  g&snake.~ (fmap (nextCoord g^.dir) g^.snake)

-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet freezes game
turn :: Direction -> Game -> Game
turn d g = g&dir.~d


nextCoord :: Direction -> Coord -> Coord
nextCoord d c = case d of
  North -> c + Coord  0 -1
  South -> c + Coord  0  1
  East  -> c + Coord -1  0
  West  -> c + Coord  1  0



-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  fx <- getStdRandom $ randomR(0,50)
  fy <- getStdRandom $ randomR(0,50)
  let firstfood = Coord fx fy
  
  return Game { _snake  = Snake (Coord 0 0)
              , _dir    = South
              , _food   = firstFood
              , _foods  = foods
              , _dead   = False
              , _paused = False
              , _score  = 0
              , _frozen = False
              }
