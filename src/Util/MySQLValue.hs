{-# LANGUAGE LambdaCase #-}
module Util.MySQLValue where

import Data.Typeable


valueOrNull :: Typeable a => Maybe a -> a
valueOrNull m = undefined

toMySqlValue :: Typeable a => a -> a
toMySqlValue value = undefined