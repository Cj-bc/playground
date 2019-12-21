{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgif)

data Face = Face { _contour :: Shgif
                 , _leftEye :: Shgif
                 , _rightEye :: Shgif
                 , _nose  :: Shgif
                 , _mouth :: Shgif
                 , _hair :: Shgif
                 }
makeLenses ''Face
data LR = L | R
data FacialExpression = Normal
                      | ClosingEye LR
                      | OpeningEye LR
                      | ClosingEyes
                      | OpeningEyes
                      | ClosingMouth
                      | OpeningMouth

-- | A data type that represent facial Parts state
data PartState = Opened  -- ^ The part is opened
               | Closed  -- ^ The part is closed
               | Opening -- ^ The part is opening
               | Closing -- ^ The part is closing

data AppState = AppState { _face :: Face
--                         , _mouthState :: PartState
--                         , _rightEyeState :: PartState
--                         , _leftEyeState :: PartState
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

partUI :: Shgif -> (Int, Int) -> Widget Name
partUI sgf (x, y) = translateBy (Location (x, y)) $ shgif sgf


ui :: AppState -> [Widget Name]
ui s = [partUI (f^.rightEye) (9, 15)
       , partUI (f^.nose) (21, 20), partUI (f^.mouth) (18, 24)
       , partUI (f^.leftEye) (27, 15), shgif (f^.hair) , shgif (f^.contour)]
  where
    f = s^.face


eHandler s (AppEvent Tick) = liftIO newAppState >>= continue
      where
        newAppState = AppState <$> newFace
        f = s^.face
        newFace = Face <$> (updateShgif $ f^.contour) <*> (updateShgif $ f^.leftEye) <*> (updateShgif $ f^.rightEye)
                       <*> (updateShgif $ f^.nose)    <*> (updateShgif $ f^.mouth) <*> (updateShgif $ f^.hair)
 --   where
 --       newAppState = AppState newFace newMouth newREye newLEye
 --       newLEye = case s^.leftEyeState of
 --                   OpeningMouth -> updateShgif s^.face^.leftEye
 --                   ClosingMouth -> 
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s _ = continue s


app :: App AppState TickEvent Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main :: IO ()
main = do
    -- Load resources
    e_hair <- getShgif "resources/shgif/hair.yaml"
    e_contour <- getShgif "resources/shgif/contour.yaml"
    e_leftEye <- getShgif "resources/shgif/leftEye.yaml"
    e_rightEye <- getShgif "resources/shgif/rightEye.yaml"
    e_nose <- getShgif "resources/shgif/nose.yaml"
    e_mouth <- getShgif "resources/shgif/mouth.yaml"
    let fromLeft (Left e) = e
    when (isLeft e_contour ) $ putStrLn (show $ fromLeft e_contour ) >> exitFailure
    when (isLeft e_leftEye ) $ putStrLn (show $ fromLeft e_leftEye ) >> exitFailure
    when (isLeft e_rightEye) $ putStrLn (show $ fromLeft e_rightEye) >> exitFailure
    when (isLeft e_nose    ) $ putStrLn (show $ fromLeft e_nose    ) >> exitFailure
    when (isLeft e_mouth   ) $ putStrLn (show $ fromLeft e_mouth   ) >> exitFailure
    let (Right c)  = e_contour
        (Right le) = e_leftEye
        (Right re) = e_rightEye
        (Right ns) = e_nose
        (Right m)  = e_mouth
        (Right h)  = e_hair
        face       = (Face c le re ns m h)

    lastState <- mainWithTick Nothing 1000 app $ AppState face
    return ()
