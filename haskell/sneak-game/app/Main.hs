module Main where

import Brick (customMain)
import Brick.BChan (newBChan, writeBChan)
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad(forever, void)

import UI (initGame, app, Tick(..))

main = do
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000
  g <- initGame
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  void $ customMain initialVty buildVty (Just chan) app g
