{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (.~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop)

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
                deriving (Eq)

data AppState = AppState { _face :: Face
                         , _rightEyeState :: PartState
                         , _leftEyeState :: PartState
                         , _mouthState :: PartState
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

partUI :: Shgif -> (Int, Int) -> Widget Name
partUI sgf (x, y) = translateBy (Location (x, y)) $ shgif sgf


-- | Render face
--
-- *Order of parts are really important*
ui :: AppState -> [Widget Name]
ui s = [partUI (f^.rightEye) (9, 15)
       , partUI (f^.nose) (21, 20), partUI (f^.mouth) (18, 24)
       , partUI (f^.leftEye) (27, 15), shgif (f^.hair) , shgif (f^.contour)]
  where
    f = s^.face


eHandler :: AppState -> BrickEvent name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick) = continue =<< liftIO (do
                                                  nf <- newFace
                                                  return $ s&face.~ nf)
      where
        f = s^.face
        partUpdate partLens condLens = case s^.condLens of
                                       Closing -> updateShgifNoLoop         $ f^.partLens
                                       Opening -> updateShgifReversedNoLoop $ f^.partLens
                                       _       -> return $ f^.partLens
        newFace = Face <$> (updateShgif $ f^.contour)
                       <*> partUpdate leftEye leftEyeState
                       <*> partUpdate rightEye rightEyeState
                       <*> (updateShgif $ f^.nose)
                       <*> partUpdate mouth mouthState
                       <*> (updateShgif $ f^.hair)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = continue $ case s^.rightEyeState  of
                                                                  Opened  -> s&rightEyeState.~ Closing
                                                                  Opening -> s&rightEyeState.~ Closing
                                                                  Closed  -> s&rightEyeState.~ Opening
                                                                  Closing -> s&rightEyeState.~ Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = continue $ case s^.leftEyeState of
                                                                  Opened  -> s&leftEyeState.~ Closing
                                                                  Opening -> s&leftEyeState.~ Closing
                                                                  Closed  -> s&leftEyeState.~ Opening
                                                                  Closing -> s&leftEyeState.~ Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = continue $ case s^.mouthState of
                                                                  Opened  -> s&mouthState.~ Closing
                                                                  Opening -> s&mouthState.~ Closing
                                                                  Closed  -> s&mouthState.~ Opening
                                                                  Closing -> s&mouthState.~ Opening
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

    -- validate if all resources are loaded correctly
    let fromLeft (Left e) = e
    flip mapM_ [e_hair, e_contour, e_leftEye, e_rightEye, e_nose, e_mouth] $ \e ->
        when (isLeft e) $ putStrLn (show $ fromLeft e) >> exitFailure

    -- Unpack Either and construct face
    let (Right c)  = e_contour
        (Right le) = e_leftEye
        (Right re) = e_rightEye
        (Right ns) = e_nose
        (Right m)  = e_mouth
        (Right h)  = e_hair
        face       = (Face c le re ns m h)

    lastState <- mainWithTick Nothing 1000 app $ AppState face  Opened Opened Opened
    return ()
