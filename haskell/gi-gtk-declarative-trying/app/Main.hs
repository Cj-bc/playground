{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
module Main where

import           Control.Monad                  ( void )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           GI.Gtk                         ( Window(..)
                                                , Box(..)
                                                , Label(..)
                                                , ListBox(..)
                                                , ListBoxRow(..)
                                                , Button(..)
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import qualified Data.Vector as V

data TodoState = TODO | DONE deriving (Show)

data TodoItem = TodoItem { todoTitle :: Text
                         , todoState :: TodoState
                         , todoDescription :: Text
                         }
type State = V.Vector TodoItem

data Event = AddItem { _item_name :: Text
                     , _item_desc :: Text
                     }
             | DoneTodo Text -- ^ use title to specify.
             | RemoveTodo Text -- ^ use title to specify.
             | AppClosed

update' :: State -> Event -> Transition State Event
update' s (AddItem title desc) = Transition (s V.++ [newItem]) (return Nothing)
  where
    newItem = TodoItem title TODO desc
update' s (DoneTodo title) = Transition newState (return Nothing)
  where
    newState = V.map (\i -> if todoTitle i == title
                     then i { todoState = DONE }
                     else i) s
update' s (RemoveTodo title) = Transition newState ( return Nothing)
  where
    newState = V.filter (\i -> (todoTitle i) /= title) s
update' _ AppClosed = Exit


todoWidget :: TodoItem -> Widget Event
todoWidget (TodoItem title state desc)
  = container Box [] [ widget Label [#label := (pack . show $ state)]
                     , widget Label [#label := title]
                     , widget Label [#label := desc]
                     ]
  
view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Todo List"
             , on #deleteEvent (const (True, AppClosed))
             ] $ container ListBox [] ( V.map (\i -> bin ListBoxRow [] $ todoWidget i) s )
  
-- | Making ToDo App
main = void $ run App { view = view'
                      , update = update'
                      , inputs = []
                      , initialState  = [TodoItem "Test" TODO "This is test entry"]
                        }
