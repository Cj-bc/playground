module Main where

import System.Random
import Control.Monad (void)
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import BlackJack
import BlackJack.Types
import UI.GUI

main = do
    initDeck <- shuffleDeck <$> getStdGen
    let initState = AppState (Game [] [] initDeck DealCard) Nothing
        app       = App {update = update'
                        , view = ui
                        , inputs = []
                        , initialState = initState
                        }
    void $ run app
