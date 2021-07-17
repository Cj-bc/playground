{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Functor ((<&>))
import qualified Data.Vector as V
import qualified GI.Gtk as Gtk
import GI.Gtk ( Box(..)
              , Window(..)
              )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Control.Lens (makeLenses, (^.), over, each)
import qualified Control.Lens as L
import StatusBar.Type
import StatusBar.Units.Pactl

data Event = AppClose

data State = State {_volumeSinks :: V.Vector VolumeSink -- ^ List of volume sinks
                   }
makeLenses ''State


slider :: VolumeSink -> BoxChild Event
slider = BoxChild defaultBoxChildProperties . pactlVolume 

volumeSliders :: State -> Widget Event
volumeSliders s = container Box [#orientation := Gtk.OrientationVertical
                          , #spacing := 100
                          , classes ["volume"]
                          ] $ s^.volumeSinks <&> slider


  
update' _ AppClose = Exit

main :: IO  ()
main = do
  let app = App { view = bin Window [#name := "Status-bar-gtk"
                                    , on #deleteEvent (const (True, AppClose))
                                    ] . volumeSliders
                , update = update'
                , inputs = []
                , initialState = State [(VolumeSink False 0.1), (VolumeSink False 0.8)]
                }

  void $ run app
