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
{-# LANGUAGE TemplateHaskell #-}
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
import Lens.Micro
import Lens.Micro.TH

type Second = Int
type MSec = Int
type Name = ()

-- Settings {{{
timeLimit :: MSec
timeLimit = 30

secToChange :: MSec
secToChange = 100

scoreOni = 100

scoreHuman = -100

defKeyPushRefreshTime = 50
-- }}}

data OoHAppState = OoHAppState { _isOniList :: [Bool]
                               , _score :: Int
                               , _oniShgif :: Shgif
                               , _humanShgif :: Shgif
                               , _tickRemain :: Int
                               , _chTick :: Int
                               , _pushedKey :: Maybe String
                               , _keyPushRefreshTime :: Int
                               }

makeLenses ''OoHAppState

-- UI {{{
ui :: OoHAppState -> [Widget Name]
ui s = [ gameUI s <+> vBox [scoreUI s
                           , manualUI
                           , padTop (Pad 37) (pushedKeyUI s)]
       ]


gameUI s = case (s^.isOniList) of
              []      -> border $ str "ALL Gone"
              True:_  -> border $ shgif (s^.oniShgif)
              False:_ -> border $ shgif (s^.humanShgif)

scoreUI s = border $ vBox [ str $ "score: "           ++ (show $ s^.score)
                          , str $ "ramain: "          ++ (show $ s^.tickRemain)
                          , str $ "characterRemain: " ++ (show $ length $ s^.isOniList)
                          ]

manualUI = border $ vBox [ str "操作方法"
                         , padLeft (Pad 4) $ str "<space>: 豆を投げる"
                         , padLeft (Pad 4) $ str "q: ゲームを終了"
                         ]

pushedKeyUI s = case (s^.pushedKey) of
                    (Just k)    -> border $ str k
                    Nothing     -> emptyWidget
-- }}}

-- Event {{{
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick)
    | (s^.tickRemain) <= 0 = halt s
    | otherwise  = continue =<< liftIO (do
                     -- Update Shgifs
                     -- This is needed to display (if not, it won't be shown)
                     newOni   <- updateShgif $ s^.oniShgif
                     newHuman <- updateShgif $ s^.humanShgif

                     -- [TODO]
                     -- I'm not sure, but changing order of those functions
                     -- might cause some error.
                     -- I fixed it right now, but might be.
                     let updateS         = updateShgifs . updateGameTime . updateCharacterTick
                                            . updateKeyEvent . updateIsOniList

                         updateIsOniList = if (s^.chTick) <= 0
                                             then over isOniList tail
                                             else id
                         updateCharacterTick = if (s^.chTick) <= 0
                                                 then set chTick secToChange
                                                 else over chTick (\x -> x - 1)

                         -- Don't change order of those functions
                         --

                         -- TODO:
                         --  WIP below
                         updateKeyEvent   = updatePushedKey . updateKeyPushRefreshTime
                         updatePushedKey  = if (s^.keyPushRefreshTime) <= 0
                                              then set pushedKey Nothing
                                              else id
                         updateKeyPushRefreshTime = if (s^.keyPushRefreshTime) <= 0
                                                      then set keyPushRefreshTime 0
                                                      else over keyPushRefreshTime (subtract 1)

                         updateGameTime   = over tickRemain (subtract 1)

                         updateShgifs     = updateOniShgif . updateHumanShgif
                         updateOniShgif   = set oniShgif newOni
                         updateHumanShgif = set humanShgif newHuman

                     return $ updateS s
                    )
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar ' ') [])) = continue $ updateS s
    where
        updateS        = updateList . updateKeyEvent . chTickReset . calcScore
        updateKeyEvent = pushedKeySet . resetRefTime

        pushedKeySet   = set pushedKey (Just "<SPACE>")
        resetRefTime   = set keyPushRefreshTime defKeyPushRefreshTime

        chTickReset    = set chTick secToChange
        doesHitOni     = head $ s^.isOniList
        calcScore      = if doesHitOni
                           then over score (+ scoreOni)
                           else over score (+ scoreHuman)
        updateList     = over isOniList tail
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar k) [])) = continue $ (s&pushedKey.~(Just [k]))
                                                                  &keyPushRefreshTime.~defKeyPushRefreshTime
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
    onilist <- replicateM 100 (randomIO :: IO Bool)
    oshgif <- getShgif "resources/oni.yaml"
    hshgif <- getShgif "resources/human.yaml"

    guard $ isRight oshgif
    guard $ isRight hshgif

    let (Right oshgif') = oshgif
        (Right hshgif') = hshgif

    lastS <- mainWithTick Nothing 1000 app $ OoHAppState onilist 0 oshgif' hshgif' (60 * 60) secToChange (Nothing) 0
    putStrLn $ "Last Score: " ++ (show $ lastS^.score)
