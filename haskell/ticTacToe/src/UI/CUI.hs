module UI.CUI where

import Brick
import Brick.Widgets.Border (border)
import Brick.Widgets.Center
import qualified Graphics.Vty as Vty
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import TicTacToe

data Name = Cell Int deriving (Show, Eq, Ord)

cell c n = clickable (Cell n) $ border $ padLeftRight 5 $ padTopBottom 2 $ str $ toStr c

board (Board b) = let first   = take 3 b
                      second  = drop 3 $ take 6 b
                      third   = drop 6 b
                  in vBox [ hBox $ zipWith cell first [0,1,2]
                          , hBox $ zipWith cell second [3,4,5]
                          , hBox $ zipWith cell third [6, 7, 8]
                          ]

view' :: (Board, Mark) -> [Widget Name]
view' (b, m) = [center $ board b]


eHandler :: (Board, Mark) -> BrickEvent Name e -> EventM Name (Next (Board, Mark))
eHandler (b, m) (VtyEvent (Vty.EvKey (Vty.KChar 'q') []))     = halt (b, m)
eHandler (b, O) (MouseDown (Cell n) _ _ _) = let newB = putMark O b n
                                             in if isFinished newB
                                                then halt (newB, O)
                                                else continue (newB, X)
eHandler (b, X) (MouseDown (Cell n) _ _ _) = let newB = putMark X b n
                                             in if isFinished newB
                                                then halt (newB, X)
                                                else continue (newB, O)
eHandler s _ = continue s


app :: App (Board, Mark) e Name
app = App { appDraw = view'
          , appHandleEvent = eHandler
          , appStartEvent = \s -> do
                vty <- getVtyHandle
                let output = Vty.outputIface vty
                when (Vty.supportsMode output Vty.Mouse) $
                    liftIO $ Vty.setMode output Vty.Mouse True
                return s
          , appChooseCursor = neverShowCursor
          , appAttrMap = const $ attrMap Vty.defAttr []
          }
