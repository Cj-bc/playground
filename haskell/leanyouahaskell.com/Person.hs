module Person
(Person(..)
) where

data Person = Person { firstName :: String
                     , lastName  :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving Show
