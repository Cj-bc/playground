{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}


module UI.GUI where

import Data.Text (pack)
import qualified Data.Vector as Vector
import GI.Gtk (Box (..), Button (..),
               Label (..),
               Window (..), ListBoxRow(..), ListBox(..), Calendar(..)
              , DrawingArea(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

import BlackJack.Types
import BlackJack

data Event = Closed
           | HitClicked
           | StandClicked


-- card :: Card -> Container Box BoxChild Event
card c = container Box [] [ BoxChild defaultBoxChildProperties { padding = 10 }
                         $ widget Label [#label := (pack $ show c)]]

-- -- informationBox :: Phase -> Container Box BoxChild Event
-- informationBox PlayerTurn  = createBox [ "Click deck: Hit", "Click stand: stand" ]
-- informationBox DealerTurn  = createBox [ "Dealer's turn. Press any key to continue"]
-- -- informationBox (GameEnd p) = createBox ["Game End", "winner: " <> (show p)]
-- informationBox (GameEnd p) = createBox [ "Game End", "winner: Shorten" ]
-- informationBox _           = createBox []

-- createBox :: [String] -> Container Box BoxChild Event
-- createBox ss = container Box [] $ map (\s -> widget Label [#label := pack s]) ss
-- createBox ss = container Box [ widget Label [ #label := "this is mock!!" ]]


ui :: AppState -> AppView Window Event
ui (AppState g _) = bin Window
                        [ #title := "BlackJack"
                        , on #deleteEvent (const (True, Closed))
                        , #widthRequest := 500
                        , #heightRequest := 400
                        ]
                        $ container ListBox []
--                            [ dealerCards
--                            , playerCards
                            [ bin ListBoxRow [] $ standClickable
                            , bin ListBoxRow [] $ deckClickable
                            ]
    where
        -- dealerCards = container Box [] (widget Label [ #label := pack ("dealer: " <> (show $ getPoint (dealer g)))])
        --                                : map card (dealer g)
        -- playerCards = container Box [] (widget Label [ #label := pack ("player: " <> (show $ getPoint (player g)))])
        --                                : map card (player g)
        standClickable = widget Button [ #label := "Stand"
                                       , on #clicked StandClicked ]
        deckClickable = widget Button [ #label := " #  "
                                      , on #clicked HitClicked ]



update' :: AppState -> Event -> Transition AppState Event
update' _ Closed = Exit
update' (AppState g _) HitClicked   = let g' = if (phase g) == PlayerTurn
                                               then doPhase $ AppState g $ Just Hit
                                               else g
                                      in Transition (AppState g' Nothing) (return Nothing)
update' (AppState g _) StandClicked = let g' = if (phase g) == PlayerTurn
                                               then doPhase $ AppState g $ Just Stand
                                               else g
                                      in Transition (AppState g' Nothing) (return Nothing)

