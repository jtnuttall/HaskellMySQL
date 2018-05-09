{-# LANGUAGE OverloadedStrings #-}
module Model.Object where

import Data.Aeson (ToJSON(..), FromJSON(..), object, pairs, withObject, (.=), (.:))
import Data.Text (Text)
import Util.String (toSnake)
import Data.String (IsString)

newtype (ToJSON a, FromJSON a) => Object a = Object
    { value :: Maybe a }
    deriving (Show)

instance (ToJSON a, FromJSON a) => ToJSON (Object a) where
    toJSON (Object v) = object [ "value" .= v ]

    toEncoding (Object v) = pairs ( "value" .= v )

instance (ToJSON a, FromJSON a) => FromJSON (Object a) where
    parseJSON = withObject "Object" $ \v -> Object
        <$> v .: "value"