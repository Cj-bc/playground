{-|
Module      : Main
Description : tiny CUI game for SETUBUN
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2020
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

This is tiny CUI game for setubun, a Japanese festival.

Oni, mostly like Japanese ogre, visit each house in the festival.
You should throw 'FUKUMAME', special soy-bean, to them to exorcise them

Beginning of the game, 'ONI' or 'Human' is appear.

If 'ONI', throw FUKUMAME by pressing Space!

If 'Human', don't throw them!

30 seconds for one game.

-}
module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Core
import Brick.Widgets.Border
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Control.Monad (replicateM, guard)
import Control.Monad.IO.Class (liftIO)
import Data.Either (isRight)
import Shgif.Type (Shgif, getShgif, updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop)
import System.Random

type Second = Int
type MSec = Int
type Name = ()

timeLimit :: MSec
timeLimit = 30

secToChange :: MSec
secToChange = 100

scoreOni = 100

scoreHuman = -100

data OoHAppState = OoHAppState { isOniList :: [Bool]
                               , score :: Int
                               , oniShgif :: Shgif
                               , humanShgif :: Shgif
                               , tickRemain :: Int
                               , chTick :: Int
                               }

-- UI {{{
ui :: OoHAppState -> [Widget Name]
ui s = [gameUI s <+> scoreUI s]

gameUI s = case isOniList s of
              []      -> border $ str "ALL Gone"
              True:_  -> border $ shgif (oniShgif s)
              False:_ -> border $ shgif (humanShgif s)

scoreUI s = border $ vBox [str $ "score: " ++ (show $ score s)
                          , str $ "ramain: " ++ (show $ tickRemain s)
                          , str $ "characterRemain: " ++ (show $ length $ isOniList s)
                          ]
-- }}}

-- Event {{{
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick) | (tickRemain s) <= 0 = halt s
                           | otherwise  = continue =<< liftIO (do
                newOni <- updateShgif $ oniShgif s
                newHuman <- updateShgif $ humanShgif s
                let ls = if (chTick s) <= 0 then (tail $ isOniList s)
                                            else (isOniList s)
                    tk = if (chTick s) <= 0 then secToChange else (chTick s - 1)
                return $ OoHAppState ls (score s) newOni newHuman (tickRemain s - 1) tk)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) = continue $
    case (isOniList s) of
        True:xs  -> OoHAppState xs (scoreOni + score s) (oniShgif s) (humanShgif s) (tickRemain s) secToChange
        False:xs -> OoHAppState xs (scoreHuman + score s) (oniShgif s) (humanShgif s) (tickRemain s) secToChange

eHandler s _ = continue s
-- }}}


app :: App OoHAppState TickEvent Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }



main :: IO ()
main = do
    onilist <- replicateM 1000 (randomIO :: IO Bool)
    oshgif <- getShgif "resources/oni.yaml"
    hshgif <- getShgif "resources/human.yaml"

    guard $ isRight oshgif
    guard $ isRight hshgif

    let (Right oshgif') = oshgif
        (Right hshgif') = hshgif

    lastS <- mainWithTick Nothing 1000 app $ OoHAppState onilist 0 oshgif' hshgif' (60 * 60) secToChange
    putStrLn $ "Last Score: " ++ (show $ score lastS)
