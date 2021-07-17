{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad (void)
import Data.Functor ((<&>))
import qualified GI.Gtk as Gtk
import GI.Gtk ( Box(..)
              , Window(..)
              )
import GI.Gtk.Declarative
import GI.Gtk.Declarative.App.Simple
import Control.Lens (makeLenses, (^.), over, each)
import StatusBar.Type

data Event = AppClose

slider :: Percentage -> BoxChild Event
slider p = BoxChild defaultBoxChildProperties $ widget Gtk.ProgressBar [#fraction := p]

sliders :: State -> Widget Event
sliders s = container Box [#orientation := Gtk.OrientationVertical
                          , #spacing := 100
                          ] $ [0.1, 0.3, 0.01] <&> slider
-- sliders s = container Box [] $ over each (\i -> i^.currentVolumePercentage) (s^.volumeSinks) <&> slider

update' _ AppClose = Exit

  

main :: IO  ()
main = do
  let app = App { view = bin Window [#name := "Status-bar-gtk"
                                    , on #deleteEvent (const (True, AppClose))
                                    ] . sliders
                , update = update'
                , inputs = []
                , initialState = State [(VolumeSink False 0.1), (VolumeSink False 0.8)]
                }

  void $ run app
