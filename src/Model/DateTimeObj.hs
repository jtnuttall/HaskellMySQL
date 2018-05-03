{-# LANGUAGE TemplateHaskell #-}
module Model.DateTimeObj where

import Util.String (toSnake)
import Data.Aeson.TH

data DateTimeObj = DateTimeObj
    { value :: Maybe String } 
    deriving (Show)

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''DateTimeObj )