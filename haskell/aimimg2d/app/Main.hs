module Main where

import System.Random (randomR, newStdGen)
import Brick (App(..), defaultMain, Widget
             , padRight, Padding(..)
             , BrickEvent(..), EventM
             , Next, halt)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Center as C
import Control.Lens

app = App

main = return ()

-- Model {{{

init :: IO Model
init = do
  x <- randomR (0, 100)
  y <- randomR (0, 100)
  return $ Model 0 (x, y) $ 5 * 60


type Coord = (Int, Int)

data Stream a = a :| Stream a

data Model = Model { _score :: Int
                   , _target :: Coord
                   , _targets :: Stream Coord
                   , _time :: Int
                   }

makeLenses ''Model
-- }}}
--
--
-- update {{{

data Tick = Tick
data Name = Target

update :: Model -> BrickEvent Name Tick -> EventM Name (Next Model)
update m (AppEvent Tick)               = if m.time == 0
                                              then halt m
                                              else continue $ reduceTime m
update m (MouseDown Target button _ _) = continue <$> hit m
update m (VtyEvent (V.EvKey (V.KChar 'q')) []) = halt m
update m _ = continue m


reduceTime :: Model -> Model
reduceTime m = m {time = time - 1}

hit :: Model -> Model
hit m = flip execState m $ do
        (t:|ts) <- use targets
        target .= t
        targets .= ts
        score +~ 1
-- }}}

-- view {{{



view :: Model -> [Widget Name]
view m =
  [C.center $ padRight (Pad 2)]
-- }}}
