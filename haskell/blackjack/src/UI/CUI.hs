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
card c = border $ str (show c)

informationBox :: Phase -> Widget Name
informationBox PlayerTurn  = border $ vBox [str "Click deck/press 'Enter': Hit", str "Click stand/press 's': stand"]
informationBox DealerTurn  = border $ str "Dealer's turn. Press any key to continue"
informationBox (GameEnd p) = border $ vBox [str "Game End", str ("winner: " <> (show p))]
informationBox _           = border $ str ""

ui :: AppState -> [Widget Name]
ui (AppState g _) = [vCenter $ vBox [dealerCards
                                    , hCenter (hBox [deckClickable, standBox])
                                    , playerCards
                                    , informationBox (phase g)]]
    where
        dealerCards   = hCenter $ hBox $ (str "dealer: ") : map card (dealer g)
        playerCards   = hCenter $ hBox $ (str "   you: ") : map card (player g)
        standBox      = clickable StandClicked $ border $ str "Stand"
        deckClickable = clickable HitClicked   $ border $ vBox $ replicate 5 $ str "##########"


askAction :: Action -> IO Action
askAction Hit   = pure Hit
askAction Stand = pure Stand

eHandler :: AppState -> BrickEvent Name e -> EventM Name (Next AppState)
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') []))     = halt s
eHandler s@(AppState g a) (VtyEvent (Vty.EvKey (Vty.KEnter) []))
                                                         = if (phase g) == PlayerTurn
                                                           then continue =<< (liftIO $ flip AppState a <$> doPhase s)
                                                           else continue s
eHandler s@(AppState g a) (VtyEvent (Vty.EvKey (Vty.KChar 's') []))
                                                         = if (phase g) == PlayerTurn
                                                           then continue =<< (liftIO $ flip AppState a <$> doPhase s)
                                                           else continue s
eHandler s@(AppState g a) (MouseDown HitClicked _ _ _)   = if (phase g) == PlayerTurn
                                                           then continue =<< (liftIO $ flip AppState a <$> doPhase (AppState g
                                                                                                                    (askAction Hit)))
                                                           else continue s
eHandler s@(AppState g a) (MouseDown StandClicked _ _ _) = if (phase g) == PlayerTurn
                                                           then continue =<< (liftIO $ flip AppState a <$> doPhase (AppState g
                                                                                                                    (askAction Stand)))
                                                           else continue s
eHandler s@(AppState g a) _ | (phase g) == PlayerTurn    = continue s
                            | (phase g) == PlayerTurn    = continue s
eHandler s@(AppState g a) _                              = continue =<< (liftIO $ flip AppState a <$> doPhase s)



-- runGame g@(Game _ _ _ (GameEnd winner)) = return (g, winner)
-- runGame g@(Game _ _ _ DealCard) = do
--                                     g' <- doPhase $ AppState g askAction
--                                     putStrLn $ "Your hand: " ++ show (player g')
--                                         ++ " | dealer's hand: ["
--                                         ++ show (head (dealer g')) ++ " * ]"
--                                     runGame g'
-- runGame g@(Game _ _ _ PlayerTurn) = do
--                                       g' <- doPhase $ AppState g askAction
--                                       putStrLn $ "Your hand: " ++ show (player g')
--                                           ++ " | dealer's hand: ["
--                                           ++ show (head (dealer g')) ++ " * ]"
--                                       runGame g'
-- runGame g@(Game _ _ _ DealerTurn) = do
--                                       g' <- doPhase $ AppState g askAction
--                                       putStrLn $ "Your hand: " ++ show (player g')
--                                           ++ " | dealer's hand: "
--                                           ++ show (dealer g')
--                                       runGame g'
-- runGame g = doPhase (AppState g askAction) >>= runGame






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
