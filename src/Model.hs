{-# LANGUAGE TemplateHaskell #-}
module Model where

import Util.String (toSnake)
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Aeson.TH (deriveJSON, defaultOptions)

type Year = ByteString
type Date = ByteString

data DateTimeWrap = DateTimeWrap
    { date :: Maybe ByteString } 
    deriving (Show)

data Url = Url
    { url :: ByteString
    , title :: ByteString
    } deriving (Show)

data Author = Author
    { bio :: Maybe ByteString
    , name :: ByteString
    , links :: Maybe [Url] 
    , personalName :: Maybe ByteString
    , deathDate :: Maybe Year
    , alternateNames :: Maybe [ByteString]
    -- , photos :: Maybe [Int]
    , created :: Maybe DateTimeWrap
    , lastModified :: DateTimeWrap
    , latestRevision :: Maybe Int
    , key :: ByteString
    , birthDate :: Maybe Year
    , revision :: Int
    } deriving (Show)

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''DateTimeWrap )
$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Url )
$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Author )
