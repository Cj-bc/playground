{-# LANGUAGE TemplateHaskell #-}
module StatusBar.Type where
import Control.Lens (makeLenses)
import qualified Data.Vector as V

type Percentage = Double

data VolumeSink = VolumeSink { _muted :: Bool
                             , _currentVolumePercentage :: Percentage
                             }
makeLenses ''VolumeSink

data State = State {_volumeSinks :: V.Vector VolumeSink -- ^ List of volume sinks
                   }
makeLenses ''State
