module My.Playground.YesNo
( YesNo
, yesno
) where

class YesNo a where
        yesno :: a -> Bool

instance YesNo Int where
        yesno 0 = False
        yesno _ = True

instance YesNo Float where
        yesno 0.0 = False
        yesno _ = True

instance YesNo [a] where
        yesno [] = False
        yesno _ = True

instance YesNo Bool where
        yesno = id

instance YesNo (Maybe m) where
        yesno Nothing = False
        yesno _ = True
