module Main where


import Tart.Canvas
import Brick
import Ascii3d

data NoName
data Event

app :: App Ascii3dState Event NoName
app = App { appDraw = raw

          }

eventHandler :: Ascii3dState -> BrickEvent NoName Event -> EventM NoName (Next Ascii3dState)
eventHandler s (

main = flip . evalState def $
       initAscii3d 0.8 100 60
       draw
  flush

