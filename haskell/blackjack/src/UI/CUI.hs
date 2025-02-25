module UI.CUI where

import Brick.Widgets.Core
import Brick.Widgets.Center
import Brick.Widgets.Border (border)
import Brick
import qualified Graphics.Vty as Vty
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)

import BlackJack
import BlackJack.Types

data Name = Deck
          | DealerCard
          | PlayerCard
          | HitClicked
          | StandClicked deriving (Ord, Eq)

card :: Card -> Widget Name
card c = border $ padAll 5 $ str (show c)

informationBox :: Phase -> Widget Name
informationBox PlayerTurn  = border $ vBox [str "Click deck/press 'Enter': Hit", str "Click stand/press 's': stand"]
informationBox DealerTurn  = border $ str "Dealer's turn. Press any key to continue"
informationBox (GameEnd p) = border $ vBox [str "Game End", str ("winner: " <> (show p))]
informationBox _           = border $ str ""

ui :: AppState -> [Widget Name]
ui (AppState g _) = [vCenter $ vBox [dealerCards
                                    , padTopBottom 4 $ hCenter (hBox [deckClickable, standBox])
                                    , playerCards
                                    , informationBox (phase g)]]
    where
        dealerCards   = hCenter $ vBox [str ("dealer: " <> (show $ getPoint (dealer g))), hBox $ map card (dealer g)]
        playerCards   = hCenter $ vBox [hBox $ map card (player g), str ("player: " <> (show $ getPoint (player g)))]
        standBox      = padTopBottom 2 $ clickable StandClicked $ border $ str "Stand"
        deckClickable = clickable HitClicked   $ border $ vBox $ replicate 11 $ str $ concat $ replicate 11 "#"


eHandler :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') []))     = halt s
eHandler (AppState g a) (VtyEvent (Vty.EvKey (Vty.KEnter) []))
                                                         = let g' = if (phase g) == PlayerTurn
                                                                    then doPhase $ AppState g $ Just Hit
                                                                    else g
                                                           in continue $ AppState g' a
eHandler s@(AppState g a) (MouseDown HitClicked _ _ _)   = let g' = if (phase g) == PlayerTurn
                                                                    then doPhase $ AppState g $ Just Hit
                                                                    else g
                                                           in continue $ AppState g' a
eHandler s@(AppState g a) (VtyEvent (Vty.EvKey (Vty.KChar 's') []))
                                                         = let g' = if (phase g) == PlayerTurn
                                                                    then doPhase $ AppState g $ Just Stand
                                                                    else g
                                                           in continue $ AppState g' a
eHandler s@(AppState g a) (MouseDown StandClicked _ _ _) = let g' = if (phase g) == PlayerTurn
                                                                    then doPhase $ AppState g $ Just Stand
                                                                    else g
                                                           in continue $ AppState g' a
eHandler s@(AppState g a) _ | (phase g) == PlayerTurn    = continue s
                            | otherwise                  = continue $ AppState (doPhase s) a


app :: App AppState e Name
app = App { appDraw = ui
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
