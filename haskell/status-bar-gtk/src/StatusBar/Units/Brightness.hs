{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
module StatusBar.Units.Brightness where

import Control.Concurrent (threadDelay)
import Control.Lens (makeLenses)
import Control.Monad (forever)
import GI.Gtk (ProgressBar(..))
import GI.Gtk.Declarative
import Pipes ( Producer
             , yield
             , lift
             )
import StatusBar.Type
import System.IO (readFile)

-- | Contains all information to show.
data Brightness = Brightness { _percentage :: Percentage
                             -- | Memo: This isn't used in any way,
                             -- but exist just so that 'percentage' lens can be generated successfully.
                             -- Otherwise it won't be correct one.
                             --
                             -- TODO: Write Lens by myself so that I can remove '_a'
                             , _a :: Percentage
                             }
makeLenses ''Brightness

  
procFile = "/sys/class/backlight/intel_backlight/brightness"
maxBrightnessFile = "/sys/class/backlight/intel_backlight/max_brightness"
  
-- | Produce 'Brightness' from system files each 1 second.
brightnessP :: (Brightness -> event) -- ^ Event constructor that will be used
  -> Producer event IO ()
brightnessP event = do
  maxB <- lift $ read . filter (/= '\n') <$> readFile maxBrightnessFile
  currentB <- lift $ read . filter (/= '\n') <$> readFile procFile
  yield . event $ Brightness (currentB/maxB) 0

  lift $ threadDelay (1000 * 100)
  brightnessP event

brightnessSlider :: Brightness -> Widget event
brightnessSlider br = widget ProgressBar [classes ["brightness"], #fraction := _percentage br]
