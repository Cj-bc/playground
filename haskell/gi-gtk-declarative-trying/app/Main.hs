{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import           Control.Monad                  ( void )
import           Data.Text                      ( Text
                                                , pack
                                                )
import Data.UUID    (UUID)
import qualified Data.UUID as UUID
import Data.UUID.V4 (nextRandom)
import           GI.Gtk                         ( Window(..)
                                                , Box(..)
                                                , Label(..)
                                                , ListBox(..)
                                                , ListBoxRow(..)
                                                , Button(..)
                                                , Entry(..)
                                                , Separator(..)
                                                , Grid(..)
                                                )
import qualified GI.Gtk as Gtk
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import qualified GI.Gtk.Declarative.Container.Grid as Grid
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Lens (ix, over, (.~), (&), (^.), (%~))
import qualified Control.Lens as Lens
import Control.Lens.TH (makeLenses)

data TodoState = TODO | DONE deriving (Show, Eq)

data TodoItem = TodoItem { _todoTitle :: Text
                         , _todoState :: TodoState
                         , _todoDescription :: Text
                         }
makeLenses ''TodoItem
  
data State = AppState { _newItemName :: Text
                      , _newItemDesc :: Text
                      , _items :: (M.Map UUID TodoItem)
                      }
makeLenses ''State

-- | Smart constructor for 'TodoItem'
newItem title desc = TodoItem title TODO desc


-- | Names to 
data EntryName = NewItemNameEntry
               | NewItemDescEntry

data Event = AddItem UUID -- ^ New item. UUID should be generated in IO monad, therefore I need to make it here.
             | DoneTodo UUID -- ^ use title to specify.
             | RemoveTodo UUID -- ^ use title to specify.
             | OnEntryChanged EntryName Text -- ^ Called when entry is modified
             | AppClosed

update' :: State -> Event -> Transition State Event
update' s (AddItem uuid) = Transition (clearEntries $ addItem s) (return Nothing)
  where
    addItem = over items (M.insert uuid $ newItem (s^.newItemName) (s^.newItemDesc))
    clearEntries = Lens.set newItemName "" . Lens.set newItemDesc ""
update' s (DoneTodo uuid) = Transition newState (return Nothing)
  where
    newState = Lens.set (items.ix uuid.todoState) (DONE) s
update' s (RemoveTodo uuid) = Transition newState ( return Nothing)
  where
    newState = over items (M.delete uuid) s
update' s (OnEntryChanged en t) = Transition (s&l.~t) (return Nothing)
  where
    l = case en of
      NewItemNameEntry -> newItemName
      NewItemDescEntry -> newItemDesc
update' _ AppClosed = Exit


-- | 'Widget' for one 'TodoItem'
todoWidget :: UUID -> TodoItem -> Widget Event
todoWidget uuid (TodoItem title state desc)
  = container Grid [#rowSpacing := 2, #columnSpacing := 10]
    $ [ Grid.GridChild { Grid.child = widget Label [#useMarkup := True
                                                   , #label := ("<s>" <> title <> "</s>")
                                                   ]
                  , Grid.properties = Grid.defaultGridChildProperties { Grid.width = 8, Grid.height = 1, Grid.topAttach = 1}
                  }
      , Grid.GridChild { Grid.child = widget Label [#useMarkup := True
                                                   , #label := ("<small>" <> desc <> "</small>")
                                                   , #xalign := 0.1]
                  , Grid.properties = Grid.defaultGridChildProperties { Grid.width = 8, Grid.height = 1, Grid.topAttach = 2}
                  }
                       ] V.++ doneButton
  where
    doneButton | state == DONE = []
               | otherwise = [Grid.GridChild { Grid.child = widget Button [#label := "mark as done", on #clicked (DoneTodo uuid)]
                                        , Grid.properties = Grid.defaultGridChildProperties { Grid.width = 1, Grid.height = 2, Grid.leftAttach = 10 }
                                        }]

-- | 'Widget' for List of toodes
todoesWidget :: State -> Widget Event  
todoesWidget = container ListBox [] . mapToVector . M.mapWithKey (\k i -> bin ListBoxRow [] $ todoWidget k i) . Lens.view items
  where
    mapToVector :: M.Map k v -> V.Vector v
    mapToVector = V.fromList . fmap snd . M.toList

newItemWidget :: State -> Widget Event
newItemWidget s = container Box [] [ widget Entry [ #placeholderText := "title"
                                                  , #text := ( s^.newItemName )
                                                  , onM #changed (fmap (OnEntryChanged NewItemNameEntry) . Gtk.entryGetText)]
                                   , widget Entry [ #placeholderText := "description"
                                                  , #text := ( s^.newItemDesc )
                                                  , onM #changed (fmap (OnEntryChanged NewItemDescEntry) . Gtk.entryGetText)]
                                   , widget Button [#label := "New item"
                                                   , onM #clicked (const (AddItem <$> nextRandom))]
                                   ]
                                 

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Todo List"
             , on #deleteEvent (const (True, AppClosed))
             ] $ container ListBox [] [ bin ListBoxRow [] $ todoesWidget s
                                      , bin ListBoxRow [] $ widget Separator []
                                      , bin ListBoxRow [] $ newItemWidget s
                                      ]
  
-- | Making ToDo App
main = void $ run App { view = view'
                      , update = update'
                      , inputs = []
                      , initialState  = AppState "" "" $ M.fromList [(UUID.nil, TodoItem "Test" TODO "This is test entry" )]
                      }
