{-# LANGUAGE TemplateHaskell #-}
module Model.AuthorRef where

import qualified Model.Author as Author (Author(..))
import Data.Aeson.TH
import Util.String (toSnake)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

newtype KeyType = KeyType 
    { key :: String }
    deriving (Show)

newtype AuthorRef = AuthorRef
    { author :: Maybe KeyType } 
    deriving (Show)

refKey :: AuthorRef -> Maybe ByteString
refKey ref = pack . key <$> author ref

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''KeyType )
$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''AuthorRef )