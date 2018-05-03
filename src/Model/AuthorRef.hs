{-# LANGUAGE TemplateHaskell #-}
module Model.AuthorRef where

import Data.Aeson.TH
import Util.String (toSnake)

data KeyType = KeyType 
	{ key :: String }
	deriving (Show)

data AuthorRef = AuthorRef
    { author :: Maybe KeyType } 
    deriving (Show)

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''KeyType )
$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''AuthorRef )