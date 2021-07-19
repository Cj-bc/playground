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
import StatusBar.Units.Brightness

data Event = AppClose
           | UpdateBrightness Brightness

data State = State {_volumeSinks :: V.Vector VolumeSink -- ^ List of volume sinks
                   , _brightness :: Brightness
                   }
makeLenses ''State



  
update' _ AppClose = Exit
update' s (UpdateBrightness br) = Transition (s L.& brightness L..~br) (return Nothing)

main :: IO  ()
main = do
  let app = App { view = bin Window [#name := "Status-bar-gtk"
                                    , on #deleteEvent (const (True, AppClose))
                                    ] . brightnessSlider . L.view brightness
                , update = update'
                , inputs = [brightnessP UpdateBrightness]
                , initialState = State [VolumeSink False 0.1, VolumeSink False 0.8] (Brightness 0.2 0)
                }

  void $ run app
