{-# LANGUAGE TemplateHaskell #-}
module Model.GenericObj where

import Data.Aeson.TH
import Data.Text
import Util.String (toSnake)

data GenericObj = GenericObj
	{ value :: Maybe Text }
	deriving (Show)


$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''GenericObj )