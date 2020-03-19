{-# LANGUAGE OverloadedStrings #-}
module Data.Hjq.Query where

import Control.Monad (join)
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import Data.Aeson
import Data.Aeson.Lens
import Data.Hjq.Parser
import Lens.Micro ((^?))

applyFilter :: JqFilter -> Value -> Either T.Text Value
applyFilter JqNil v = Right v
applyFilter (JqField fieldName f) v = join $ noteKeyNotFoundError fieldName (fmap (applyFilter f) (v ^? key fieldName))
applyFilter (JqIndex idx       f) v = join $ noteIndexOutOfRangeError idx (fmap (applyFilter f) (v ^? nth idx))
applyFilter f                     v = Left $ "unexpected pattern: " <> tshow f <> " : " <> tshow v

noteKeyNotFoundError :: T.Text -> Maybe a -> Either T.Text a
noteKeyNotFoundError _ (Just v) = Right v
noteKeyNotFoundError s Nothing  = Left $ "Key not found: " <> s

noteIndexOutOfRangeError :: Int -> Maybe a -> Either T.Text a
noteIndexOutOfRangeError _ (Just e) = Right e
noteIndexOutOfRangeError i Nothing  = Left $ "Index out of range: " <> tshow i

tshow :: Show a => a -> T.Text
tshow = T.pack . show


executeQuery :: JqQuery -> Value -> Either T.Text Value
executeQuery (JqQueryObject o) v = fmap (Object . H.fromList) . sequence . fmap sequence . fmap (fmap (flip executeQuery v)) $ o
executeQuery (JqQueryArray l)  v = fmap (Array . V.fromList) . sequence $ fmap (flip executeQuery v) l
executeQuery (JqQueryFilter f) v = applyFilter f v
