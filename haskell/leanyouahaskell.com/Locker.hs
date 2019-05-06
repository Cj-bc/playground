module Locker
( LockerState
, LockerMap
, lockerLookup
, Code
, lockers
) where

import qualified Data.Map as Map

data LockerState = Taken | Free deriving (Eq, Show)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber lockermap = case Map.lookup lockerNumber lockermap of
                                        Nothing -> Left "The locker does not exist"
                                        Just (Taken, _) -> Left "The locker has been taken"
                                        Just (Free, code) -> Right code

lockers :: LockerMap
lockers = Map.fromList
                [(100, (Taken, "ZD39I"))
                ,(101, (Free, "JAH3I"))
                ,(103, (Free, "IQSA9"))
                ,(105, (Free, "QOTSA"))
                ,(109, (Taken, "893JJ"))
                ,(110, (Taken, "99292"))
                ]
