{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens (makeLenses, (^.), (&), (.~), over, set)
import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import System.Exit (exitFailure, exitSuccess)
import System.Environment (getArgs)
import Data.Either (isLeft)
import qualified Graphics.Vty as Vty
import Brick
import Brick.Widgets.Border (border)
import Brick.Extensions.Shgif.Widgets (shgif, canvas)
import Brick.Extensions.Shgif.Events (TickEvent(..), mainWithTick)
import Shgif.Type (Shgif, getShgif, updateShgifNoLoop, updateShgif, updateShgifReversedNoLoop
                  , updateShgifTo, shgifToCanvas, width, height)
import Tart.Canvas

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

-- data types {{{
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
                         , _noseOffset :: (Int, Int)
                         , _faceLooking :: Maybe LR
                         , _tick :: Int
                         , _currentCanvas :: Canvas
                         }
makeLenses ''AppState

data Name = NoName deriving (Eq, Ord)
-- }}}

-- UI {{{
-- | Render face
ui :: AppState -> [Widget Name]
ui s = [ border $ canvas [(s^.currentCanvas)]
       ]
-- }}}

-- event handler {{{
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
-- * 'o' : Look front
eHandler :: AppState -> BrickEvent name TickEvent -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent Tick) = continue =<< liftIO (do
                                                  nf <- newFace
                                                  nc <- updateCanvas
                                                  let updateOffset = if ((s^.tick) `mod` 10) == 0
                                                                       then calculateOffset
                                                                       else id
                                                  return $ over tick (+1)
                                                         $ updateOffset
                                                         $ set face nf
                                                         $ set currentCanvas nc
                                                         s
                                                 )
      where
        updateTick = over tick (+1)
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
        mouthTick = case s^.mouthState of
                     Opening -> 0
                     Opened  -> 0
                     Closing -> 50
                     Closed  -> 50
                     Emote1  -> 60
                     Emote2  -> 60
        newFace = Face <$> (updateShgif $ f^.contour)
                       <*> (updateShgifTo eyeLTick $ f^.leftEye)
                       <*> (updateShgifTo eyeRTick $ f^.rightEye)
                       <*> (updateShgif $ f^.nose)
                       <*> (updateShgifTo mouthTick $ f^.mouth)
                       <*> (updateShgif $ f^.hair)
                       <*> (updateShgif $ f^.backHair)
        calculateOffset = case (s^.faceLooking) of
                            Nothing -> calculateOffset' (0, 0) (0, 0) (0, 0) (0, 0) (0, 0)
                            Just R  -> calculateOffset' (-1, 0) (-2, 0) (-1, 0) (0, 0) (-1, 0)
                            Just L  -> calculateOffset' (2, 0) (1, 0) (1, 0) (0, 0) (1, 0)
        calculateOffset' a b c d e = over rightEyeOffset  (_moveOffsetTo a)
                                     . over leftEyeOffset (_moveOffsetTo b)
                                     . over mouthOffset   (_moveOffsetTo c)
                                     . over hairOffset    (_moveOffsetTo d)
                                     . over noseOffset    (_moveOffsetTo e)
        addOffset (a, b) (c, d) = (a + c, b + d)
        updateCanvas = mergeToBigCanvas [ (f^.rightEye, (13, 15) `addOffset` (s^.rightEyeOffset))
                                        , (f^.nose    , (25, 20) `addOffset` (s^.noseOffset))
                                        , (f^.mouth   , (22, 24) `addOffset` (s^.mouthOffset))
                                        , (f^.leftEye , (29, 15) `addOffset` (s^.leftEyeOffset))
                                        , (f^.hair    , (5, 0)   `addOffset` (s^.hairOffset))
                                        , (f^.backHair, (4, 0))
                                        ]


eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = continue $ s&rightEyeState.~Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = continue $ s&rightEyeState.~Closing
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'x') [])) = continue $ s&rightEyeState.~Emote1
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'r') [])) = continue $ s&rightEyeState.~Emote2
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = continue $ s&leftEyeState.~Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = continue $ s&leftEyeState.~Closing
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'c') [])) = continue $ s&leftEyeState.~Emote1
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'f') [])) = continue $ s&leftEyeState.~Emote2
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'm') [])) = continue $ s&mouthState.~ Closing
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) = continue $ s&mouthState.~ Opening
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'b') [])) = continue $ s&mouthState.~ Emote1
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'l') [])) = continue $ s&faceLooking.~(Just L)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'h') [])) = continue $ s&faceLooking.~(Just R)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'o') [])) = continue $ s&faceLooking.~Nothing
-- eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'n') [])) = continue $ s&rightEyeOffset.~ (0, 0)
--                                                                    &leftEyeOffset.~ (0, 0)
--                                                                    &mouthOffset.~ (0, 0)
--                                                                    &hairOffset.~ (0, 0)
eHandler s _ = continue s

_moveOffsetTo :: (Int, Int) -> (Int, Int) -> (Int, Int)
_moveOffsetTo limit@(x1, y1) current@(x2, y2) | current == limit = current
                                              | x1 == x2         = updateY current
                                              | y1 == y2         = updateX current
                                              | otherwise        = updateX $ updateY current
    where
        updateX (tx, ty) = if x2 < x1
                             then (tx + 1, ty)
                             else (tx - 1, ty)
        updateY (tx, ty) = if y2 < y1
                             then (tx, ty + 1)
                             else (tx, ty - 1)
-- }}}

-- Utilities for Canvas {{{

-- | Render Canvas into other canvas
plotToCanvas :: (Int, Int) -> Canvas -> Canvas -> IO Canvas
plotToCanvas (dw, dh) bc c = do
    let (w, h) = canvasSize c
    write [(w', h') | w' <- [0..w-1], h' <- [0..h-1]] bc
    where
        write :: [(Int, Int)] -> Canvas -> IO Canvas
        write [] bc'         = return bc'
        write ((w, h):x) bc' = do
            let (ch, attr) = canvasGetPixel c (w, h)
            case ch of
                ' ' -> write x bc
                _   -> do
                  newC <- canvasSetPixel bc (w + dw, h + dh) ch attr
                  write x newC


-- | Merge and render all Shgifs into one Canvas
mergeToBigCanvas :: [(Shgif, (Int, Int))] -> IO Canvas
mergeToBigCanvas ss = do
    emptyCanvas <- newCanvas (w, h)
    write ss emptyCanvas
    where
        w = maximum $ fmap (\(s, (w',_)) -> s^.width + w') ss
        h = maximum $ fmap (\(s, (_,h')) -> s^.height + h') ss

        -- | Write Canvases one after another
        write :: [(Shgif, (Int, Int))] -> Canvas -> IO Canvas
        write [] c         = return c
        write ((s,p):x) bc = do
            shgifC <- shgifToCanvas s
            newC <- plotToCanvas p bc shgifC
            write x newC
-- }}}






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

    emptyCanvas <- newCanvas (1, 1)
    lastState <- mainWithTick Nothing 1000 app $ AppState face  Opened Opened Opened
                                                 (0,0) (0,0) (0,0) (0, 0) (0, 0) Nothing 0
                                                 emptyCanvas
    return ()
