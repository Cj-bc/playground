{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
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
                                                )
import           GI.Gtk.Declarative
import           GI.Gtk.Declarative.App.Simple
import qualified Data.Vector as V
import qualified Data.Map as M
import Control.Lens (ix, over)

data TodoState = TODO | DONE deriving (Show, Eq)

data TodoItem = TodoItem { todoTitle :: Text
                         , todoState :: TodoState
                         , todoDescription :: Text
                         }

appendNewItem :: State -> Text -> Text -> IO State
appendNewItem s title desc = M.insert <$> nextRandom
                                      <*> return (TodoItem title TODO desc)
                                      <*> return s
  
type State = M.Map UUID TodoItem

data Event = AddItem { _item_uuid :: UUID
                     , _item_name :: Text
                     , _item_desc :: Text
                     }
             | DoneTodo UUID -- ^ use title to specify.
             | RemoveTodo UUID -- ^ use title to specify.
             | AppClosed

update' :: State -> Event -> Transition State Event
update' s (AddItem uuid title desc) = Transition (M.insert uuid newItem s) (return Nothing)
  where
    newItem = TodoItem title TODO desc
update' s (DoneTodo uuid) = Transition newState (return Nothing)
  where
    newState = over (ix uuid) (\i -> i { todoState = DONE }) s
update' s (RemoveTodo uuid) = Transition newState ( return Nothing)
  where
    newState = M.delete uuid s
update' _ AppClosed = Exit


-- | 'Widget' for one 'TodoItem'
todoWidget :: UUID -> TodoItem -> Widget Event
todoWidget uuid (TodoItem title state desc)
  = container Box [] $ [ widget Label [#label := (pack . show $ state)]
                       , widget Label [#label := title]
                       , widget Label [#label := desc]
                       ] V.++ doneButton
  where
    doneButton | state == DONE = []
               | otherwise = [widget Button [#label := "mark as done", on #clicked (DoneTodo uuid)]]

-- | 'Widget' for List of toodes
todoesWidget :: State -> Widget Event  
todoesWidget = container ListBox [] . mapToVector . M.mapWithKey (\k i -> bin ListBoxRow [] $ todoWidget k i)
  where
    mapToVector :: M.Map k v -> V.Vector v
    mapToVector = V.fromList . fmap snd . M.toList

newItemWidget :: Widget Event
newItemWidget = container Box [] [ widget Button [#label := "hoge"]
                                 ,widget Entry [ #text := "title" ]
                                 ,widget Entry [ #text := "description" ]
                                 , widget Button [#label := "New item"
                                                 , onM #clicked (const (AddItem <$> nextRandom
                                                                         <*> return "New Item"
                                                                         <*> return "Description"))]
                                 ]
                                 

view' :: State -> AppView Window Event
view' s =
  bin Window [#title := "Todo List"
             , on #deleteEvent (const (True, AppClosed))
             ] $ container ListBox [] [ bin ListBoxRow [] (todoesWidget s)
                                      , bin ListBoxRow [] newItemWidget
                                      ]
  
-- | Making ToDo App
main = void $ run App { view = view'
                      , update = update'
                      , inputs = []
                      , initialState  = M.fromList [(UUID.nil, TodoItem "Test" TODO "This is test entry" )]
                      }
