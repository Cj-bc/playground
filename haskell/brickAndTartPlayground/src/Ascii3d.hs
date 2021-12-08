{-# LANGUAGE TemplateHaskell #-}
module Ascii3d where

import Lens.Micro.Platform
import Tart.Canvas
import Data.Vector
import Data.Default
import Control.Monad.State.Strict
import Lens.Micro.Platform
import Linear.V2
import Linear.V3

type Position = V3 Float

data VertexData = VertexData { _position :: Position
                             }
makeLenses ''VertexData

data Ascii3dState = Ascii3dState { _vertexBuffer :: Vector VertexData
                                 , _cameraPos :: Position
                                 , _aspectRatio :: Float
                                 , _screenWidth :: Int
                                 , _hFov :: Float
                                 , _mainBuffer :: Maybe Canvas
                                 }
makeLenses ''Ascii3dState

instance Default Ascii3dState where
  def = Ascii3dState empty (V3 0.0 0.0 0.0) 0.8 100 60 Nothing

type Ascii3d a = StateT Ascii3dState IO a
  
initAscii3d :: Float -> Int -> Float -> Ascii3d ()
initAscii3d aspectR screenW fv = do
  let screenH = round (aspectR * (fromRational . toRational $ screenW))
  c <- lift $ newCanvas (screenW, screenH)
  put (Ascii3dState empty (V3 0.0 0.0 0.0) aspectR screenW fv (Just c))

moveCamera :: V3 Float -> Ascii3d ()
moveCamera p = cameraPos %= (+ p)

addVertex :: VertexData -> Ascii3d()
addVertex vd = vertexBuffer %= (<> (singleton vd))

draw :: Ascii3d ()
draw = do
  state <- get
  let facialLength = 1 / (tan (state^.hFov/2))
      screenBuffer = fmap (drawPoint facialLength) (state^.vertexBuffer) :: Vector (V2 Float)
      screenW = fromRational . toRational $ state^.screenWidth
      buffer = state^.mainBuffer
      halfScreen = screenW/2

  case buffer of
    Nothing -> return ()
    Just b' -> do
      newC <- lift $ canvasSetMany b' $ toList (fmap (\c -> ((round $ (c^._x)*screenW + halfScreen, round $ (c^._y)*(screenW*(state^.aspectRatio)) + halfScreen)
                                                            , '*', mempty)) screenBuffer)
      mainBuffer .= (Just newC)


drawPoint :: Float -> VertexData -> V2 Float
drawPoint flength vertex =
  let ratio = flength/(vertex^.position^._z)
  in V2 ((vertex^.position^._x)*ratio) ((vertex^.position^._y)*ratio)
