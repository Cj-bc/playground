{-# LANGUAGE TypeFamilies, TemplateHaskell #-}
{-|
Module      : HitNBlow.Type
Description : Common Type definitions for HitNBlow game
Copyright   : (c) Cj.bc_sd a.k.a Cj-bc, 2021
Maintainer  : cj.bc-sd@outlook.jp
Stability   : experimental

-}
module HitNBlow.Type where
import Control.Lens
import Control.Monad.Trans.State

-- | Represents each Pin
data Pin = Red | Blue | Green | White | Purple deriving (Show)

-- | One Set of Pins that user will guess 
data Lane = Lane (Maybe Pin) (Maybe Pin) (Maybe Pin) (Maybe Pin) (Maybe Pin)
    deriving (Show)

type instance Index Lane = Int
type instance IxValue Lane = Maybe Pin

-- | 'Control.Lens.Ixed' instance for 'Lane'
--
-- This will do nothing and return given 'Lane' as is if given index is out-of-range
instance Ixed Lane where
    ix 1 = \g l@(Lane a b c d e) -> Lane <$> g a <*> pure  b <*> pure  c <*> pure  d <*> pure  e
    ix 2 = \g l@(Lane a b c d e) -> Lane a <$> g b <*> pure  c <*> pure  d <*> pure  e
    ix 3 = \g l@(Lane a b c d e) -> Lane a b <$> g c <*> pure  d <*> pure  e
    ix 4 = \g l@(Lane a b c d e) -> Lane a b c <$> g d <*> pure  e
    ix 5 = \g l@(Lane a b c d e) -> Lane a b c d <$> g e
    ix _ = \_ l -> pure l

data Result = Solved | Failed
data GamePhase = InGame
               | End Result

-- | How many try can use do until 'Failed'?
triesLimit = 5

-- | Game State.
-- 
-- TODO: It might be better to make 'Monad' instance for this (or by using 'Free' monad)
-- So that I can write DSL
data GameState = GameState {
      _correctAnswer :: Lane
    , _tries         :: [Lane]
    , _currentChoise :: Lane
    , _phase         :: GamePhase
    }

makeLenses ''GameState

-- | Make initial 'Game' state.
--
-- Note that 
instance Default Game where
    def = Game ran

type Game = State GameState
