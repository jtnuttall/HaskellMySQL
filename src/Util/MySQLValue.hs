{-# LANGUAGE LambdaCase #-}
module Util.MySQLValue where

import Data.Typeable

valueOrNull :: Typeable a => Maybe a -> MySQLValue
valueOrNull = \case
    Just value -> toMySqlValue value
    Nothing    -> MySQLNull

toMySqlValue :: Typeable a => a -> MySQLValue
toMySqlValue value = undefined