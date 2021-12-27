{-# LANGUAGE OverloadedStrings #-}
import Tart.Canvas
import Lens.Micro.Platform
import Brick
import Brick.Util (bg)
import Brick.Widgets.Border (border)
import Brick.Widgets.Table
import Ascii3d
import qualified Graphics.Vty as V
import Graphics.Vty.Attributes.Color as VColor
import Data.Default (def)
import Control.Monad.State.Strict
import Linear.V3 (V3(..), _x, _y, _z)
import Data.Vector (toList, fromList, Vector(..), zipWith)


data NoName = NoName deriving (Ord, Eq)
data Event = NoEvent deriving (Ord, Eq)

app :: App Ascii3dState Event NoName
app = App { appDraw = ui
          , appChooseCursor = neverShowCursor
          , appHandleEvent = eventHandler
          , appStartEvent = liftIO . execStateT draw
          , appAttrMap = const $ attrMap V.defAttr [("Supported", bg VColor.green)
                                                   ,("Unsupported", bg VColor.red)]
          }


positionView :: Position -> Widget n
positionView p = border $ vBox [ str $ "x: " ++ show (p^._x :: Float)
                               , str $ "y: " ++ show (p^._y :: Float)
                               , str $ "z: " ++ show (p^._z :: Float)
                               ]

vertexInfo :: VertexData -> Widget n
vertexInfo v = hBox [ str $ "x: " <> show (v^.position^._x :: Float)
                    , str $ "y: " <> show (v^.position^._y :: Float)
                    , str $ "z: " <> show (v^.position^._z :: Float)
                    ]

verticesInfo :: Vector VertexData -> Widget n
verticesInfo vs = renderTable . table $ [str " ", str "x", str "y", str "z"] : (toList $ Data.Vector.zipWith v vs (fromList [0..]))
  where
       v p idx = [ str $ show idx
                 , str . show $ p^.position^._x
                 , str . show $ p^.position^._y
                 , str . show $ p^.position^._z
                 ]

supportedFeatures :: Widget n
supportedFeatures = withAttr "features-table" . renderTable . table $ fmap convert featureList
  where
    convert :: (Bool, String) -> [Widget n]
    convert (True, feature)  = [withAttr "Supported"   $ str " ", str feature]
    convert (False, feature) = [withAttr "Unsupported" $ str " ", str feature]
    featureList :: [(Bool, String)]
    featureList = [(True, "WASD Camera Movement")
                  ,(True, "Vertex shading")
                  ,(False, "Clipping")
                  ,(False, "Shape assembly")
                  ,(False, "Lighting")
                  ]
                  

  
ui :: Ascii3dState -> [Widget n]
ui s = case s^.mainBuffer  of
         Nothing -> [str "hello there!"]
         Just c -> [hBox [border . raw $ canvasLayersToImage [c]
                         , vBox [positionView (s^.cameraPos)
                                , hBox [ vBox [str "Vertices"
                                              , verticesInfo (s^.vertexBuffer)
                                              ]
                                       , vBox [ str "Features"
                                              , supportedFeatures
                                              ]
                                       ]
                                ]
                         ]
                   
                   ]

eventHandler :: Ascii3dState -> BrickEvent NoName Event -> EventM NoName (Next Ascii3dState)
eventHandler s (VtyEvent (V.EvKey (V.KChar 'w') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 0 0 (-1)) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KChar 's') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 0 0 1) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KChar 'a') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 (-1) 0 0) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KChar 'd') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 1 0 0) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KChar 'q') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 0 (-1) 0) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KChar 'e') [])) = do
  s' <- liftIO (execStateT (moveCamera (V3 0 1 0) >> draw) s)
  continue s'
eventHandler s (VtyEvent (V.EvKey (V.KEsc) [])) = halt s
eventHandler s _ = continue s

main = do
  initialState <- flip execStateT def $ do
                      initAscii3d 0.8 100 60
                      -- sequence $ do
                      --   x<- [-3,3]
                      --   y<- [-3,3]
                      --   z<- [-1,-3]
                      --   return $ addVertex $ VertexData (V3 x y z)
                      sequence $ do
                        x<- [-3..3]
                        y<- [-3,3]
                        z<- [-1,-3]
                        return $ addVertex $VertexData (V3 x y z)
                      sequence $ do
                        x<- [-3,3]
                        y<- [-3..3]
                        z<- [-1,-3]
                        return $ addVertex $VertexData (V3 x y z)
                      sequence $ do
                        x<- [-3,3]
                        y<- [-3,3]
                        z<- (negate) <$> [1..3]
                        return $ addVertex $VertexData (V3 x y z)
  finalState <- defaultMain app initialState
  putStrLn . show $ fmap (\v -> show $v^.position) (finalState^.vertexBuffer)
  putStrLn . show . fmap canvasSize $ finalState^.mainBuffer
  return ()
