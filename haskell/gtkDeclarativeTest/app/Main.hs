{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import Control.Monad (void)
import Data.Text (pack)
import GI.Gtk (Window(..), Label(..), Button(..), Box(..))
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple

data Event = Clicked | Closed

data State = ON | OFF deriving (Show)

view' :: State -> AppView Window Event
view' s = bin Window [ #title := "Test for Gtk declarative"
                     , on #deleteEvent (const (True, Closed))
                     , #widthRequest := 500
                     , #heightRequest := 500
                     ]
             $ container Box [] [ widget Label [#label := "Label 1"]
                                , widget Button [#label := pack (show s)
                                                , on #clicked Clicked
                                                ]
                                ]

update' :: State -> Event -> Transition State Event
update' _ Closed = Exit
update' ON Clicked  = Transition OFF (return Nothing)
update' OFF Clicked = Transition ON (return Nothing)

main = void $ run $ App {update = update'
                        , view = view'
                        , inputs = []
                        , initialState = OFF}

