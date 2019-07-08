{-# LANGUAGE TemplateHaskell #-}


module Main where

import System.Exit (exitFailure)
import Control.Monad (forever, when)
import Control.Concurrent (threadDelay, forkIO)
import System.Random (randomRs, newStdGen, Random(..))
import Control.Monad.Trans.State (execState, StateT(..), get)
import Brick (App(..), customMain, Widget, neverShowCursor
             , BrickEvent(..), EventM
             , Next, halt, continue
             , withBorderStyle, str, clickable
             , hLimit, hBox, vBox
             , padRight, padAll, Padding(..)
             , withAttr, attrName, attrMap, AttrMap(..), on
             , (<+>))
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Brick.BChan (newBChan, writeBChan)
import Control.Lens hiding (view)
import Linear.V2 (V2(..))

heigh, width :: Int
heigh = 20
width = 20


-- Model {{{

modelInit :: IO Model
modelInit = do
  (t:|ts) <- fromList . randomRs (V2 0 0, V2 (width-1) (heigh-1)) <$> newStdGen

  return $ Model 0 t ts $ 5 * 60 * 10000

instance Random a => Random (V2 a) where
  randomR (V2 x1 y1, V2 x2 y2) g =
        let (x, g')  = randomR (x1, x2) g
            (y, g'') = randomR (y1, y2) g'
        in
            (V2 x y, g'')



type Coord = V2 Int

data Stream a = a :| Stream a

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Stream must be infinite")

data Model = Model { _score :: Int
                   , _target :: Coord
                   , _targets :: Stream Coord
                   , _time :: Int
                   }

makeLenses ''Model
-- }}}

-- update {{{

data Tick = Tick
data Name = Target
          | NoName deriving (Eq, Ord)

update :: Model -> BrickEvent Name Tick -> EventM Name (Next Model)
update m (AppEvent Tick)               = if m^.time == 0
                                              then halt m
                                              else continue $ reduceTime m
update m (MouseDown _ _ _ _) = continue $ m { _time = 100}
update m (VtyEvent ((V.EvKey (V.KChar 'q') []))) = halt m
update m _ = continue m


reduceTime :: Model -> Model
reduceTime m = m&time -~ 1

-- As I didn't understand how lens works, I'm using basic way here.
hit :: Model -> Model
hit m = flip execState m $ do
        model <- get
        let oldScore = _score model
            newScore = oldScore + 1
            newTime  = _time model
            (newTarget:|newTargets) = _targets model
        return $ Model newScore newTarget newTargets newTime

-- }}}

-- view {{{
view :: Model -> [Widget Name]
view m =
  [padRight (Pad 2) (viewStat m) <+> B.vBorder <+> viewPlayArea m]

viewStat :: Model -> Widget Name
viewStat m = hLimit 11 $ vBox [ str ("time: " ++ (show (_time m)))
                              , str ("score: " ++ (show (_score m))) ]

viewTime :: Model -> Widget Name
viewTime m = str $ show $ _time m

viewScore :: Int -> Widget Name
viewScore n =  vBox
                 [ C.hCenter
                 $ padAll 1
                 $ str $ show n
                 ]

viewPlayArea :: Model -> Widget Name
viewPlayArea m =  vBox rows
        where
          rows = [hBox $ colsInRow y | y <- [heigh-1,heigh-2..0]]
          colsInRow y = [ viewCol (V2 x y) | x <- [0,1..width-1]]
          viewCol c | c == m^.target = viewTarget
                    | otherwise      = clickable Target $ withAttr (attrName "Empty") $ str " "
          viewTarget = clickable Target
                        $ withAttr (attrName "target")
                        $ str " "
-- }}}

-- attrMap {{{
cssMap :: AttrMap
cssMap = attrMap V.defAttr
  [ ((attrName "target"), V.red `on` V.red) ]

-- }}}

app = App { appDraw = view
          , appChooseCursor = neverShowCursor
          , appHandleEvent = update
          , appStartEvent = return
          , appAttrMap = const cssMap
          }


main = do
    checkForMouseSupport
    chan <- newBChan 10
    forkIO $ forever $ do
        writeBChan chan Tick
        threadDelay 1000000 -- microsec
    initStat <- modelInit
    let buildVty = V.mkVty V.defaultConfig
    initialVty <- buildVty
    finalStat <- customMain initialVty buildVty (Just chan) app initStat
    print $ "your score was: " ++ (show $ _score finalStat)


checkForMouseSupport :: IO ()
checkForMouseSupport = do
    vty <- V.mkVty =<< V.standardIOConfig

    when (not $ V.supportsMode (V.outputIface vty) V.Mouse) $ do
        putStrLn "Error: this terminal does not support mouse interaction"
        exitFailure

    V.shutdown vty
