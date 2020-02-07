{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (.~))
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Extensions.Shgif.Widgets (shgif)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop
                  , updateShgifTo)

helpText = unlines ["faceWithShgif -- prototype program to do live2d like animation with shgif"
                   , ""
                   , "Key control:"
                   , "    q: quit program"
                   , "    w: switch right eye"
                   , "    e: switch left eye"
                   , "    m: switch mouth"
                   , "    l: look left"
                   , "    h: look right"
                   ]

data Face = Face { _contour :: Shgif
                 , _leftEye :: Shgif
                 , _rightEye :: Shgif
                 , _nose  :: Shgif
                 , _mouth :: Shgif
                 , _hair :: Shgif
                 , _backHair :: Shgif
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
               | Emote1
               | Emote2
                deriving (Eq)

data AppState = AppState { _face :: Face
                         , _rightEyeState :: PartState
                         , _leftEyeState :: PartState
                         , _mouthState :: PartState
                         , _rightEyeOffset :: (Int, Int)
                         , _leftEyeOffset :: (Int, Int)
                         , _mouthOffset :: (Int, Int)
                         , _hairOffset :: (Int, Int)
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)

partUI :: Shgif -> (Int, Int) -> Widget Name
partUI sgf (x, y) = translateBy (Location (x, y)) $ shgif sgf


-- | Render face
--
-- *Order of parts are really important*
ui :: AppState -> [Widget Name]
ui s = [partUI (f^.rightEye) $ (13, 15) `addOffset` (s^.rightEyeOffset)
       , partUI (f^.nose) (25, 20)
       , partUI (f^.mouth) $ (22, 24) `addOffset` (s^.mouthOffset)
       , partUI (f^.leftEye) $ (29, 15) `addOffset` (s^.leftEyeOffset)
       , partUI (f^.hair) $ (5, 0) `addOffset` (s^.hairOffset)
       , shgif (f^.contour)
       , partUI (f^.backHair) (4, 0)
       ]
  where
    f = s^.face
    addOffset (a, b) (c, d) = (a + c, b + d)


-- | event handler
--
-- key bindings:
--
-- * 'q' : quit the app
--
-- * 'w' : Open right eye
--
-- * 's' : Close right eye
--
-- * 'x' : Open right eye wide
--
-- * 'e' : Open left eye
--
-- * 'd' : Close left eye
--
-- * 'c' : Open left eye wide
--
-- * 'm' : Open/Close mouth
--
-- * 'l' : Look left
--
-- * 'h' : Look right
--
-- * 'n' : Look front
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

        eyeRTick = case s^.rightEyeState of
                     Opening -> 40
                     Opened  -> 40
                     Closing -> 70
                     Closed  -> 70
                     Emote1  -> 0
                     Emote2  -> 90
        eyeLTick = case s^.leftEyeState of
                     Opening -> 40
                     Opened  -> 40
                     Closing -> 70
                     Closed  -> 70
                     Emote1  -> 0
                     Emote2  -> 90
        newFace = Face <$> (updateShgif $ f^.contour)
                       <*> (updateShgifTo eyeLTick $ f^.leftEye)
                       <*> (updateShgifTo eyeRTick $ f^.rightEye)
                       <*> (updateShgif $ f^.nose)
                       <*> partUpdate mouth mouthState
                       <*> (updateShgif $ f^.hair)
                       <*> (updateShgif $ f^.backHair)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = continue $ s&rightEyeState.~Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = continue $ s&rightEyeState.~Closing
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'x') [])) = continue $ s&rightEyeState.~Emote1
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = continue $ s&rightEyeState.~Emote2
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = continue $ s&leftEyeState.~Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = continue $ s&leftEyeState.~Closing
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'c') [])) = continue $ s&leftEyeState.~Emote1
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'f') [])) = continue $ s&leftEyeState.~Emote2
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'm') [])) = continue $ case s^.mouthState of
                                                                  Opened  -> s&mouthState.~ Closing
                                                                  Opening -> s&mouthState.~ Closing
                                                                  Closed  -> s&mouthState.~ Opening
                                                                  Closing -> s&mouthState.~ Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = continue $ s&rightEyeOffset.~ (3, 0)
                                                                   &leftEyeOffset.~ (2, 0)
                                                                   &mouthOffset.~ (1, 0)
                                                                   &hairOffset.~ (1, 0)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = continue $ s&rightEyeOffset.~ (-2, 0)
                                                                   &leftEyeOffset.~ (-3, 0)
                                                                   &mouthOffset.~ (-1, 0)
                                                                   &hairOffset.~ (-1, 0)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) = continue $ s&rightEyeOffset.~ (0, 0)
                                                                   &leftEyeOffset.~ (0, 0)
                                                                   &mouthOffset.~ (0, 0)
                                                                   &hairOffset.~ (0, 0)
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
    -- help message
    arg <- getArgs
    when (arg /= [] && (head arg == "--help" || head arg == "-h")) $ putStrLn helpText >> exitSuccess

    -- Load resources
    e_hair <- getShgif "resources/shgif/hair.yaml"
    e_contour <- getShgif "resources/shgif/contour.yaml"
    e_leftEye <- getShgif "resources/shgif/leftEye.yaml"
    e_rightEye <- getShgif "resources/shgif/rightEye.yaml"
    e_nose <- getShgif "resources/shgif/nose.yaml"
    e_mouth <- getShgif "resources/shgif/mouth.yaml"
    e_backHair <- getShgif "resources/shgif/hair_back.yaml"

    -- validate if all resources are loaded correctly
    let fromLeft (Left e) = e
    flip mapM_ [e_hair, e_contour, e_leftEye, e_rightEye, e_nose, e_mouth, e_backHair] $ \e ->
        when (isLeft e) $ putStrLn (show $ fromLeft e) >> exitFailure

    -- Unpack Either and construct face
    let (Right c)  = e_contour
        (Right le) = e_leftEye
        (Right re) = e_rightEye
        (Right ns) = e_nose
        (Right m)  = e_mouth
        (Right h)  = e_hair
        (Right hb) = e_backHair
        face       = (Face c le re ns m h hb)

    lastState <- mainWithTick Nothing 1000 app $ AppState face  Opened Opened Opened (0,0) (0,0) (0,0) (0, 0)
    return ()
