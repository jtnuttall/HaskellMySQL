{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Model.Author where

import Model.DateTimeObj (DateTimeObj)
import qualified Model.DateTimeObj as DateTimeObj
import Model.Url (Url, UrlQueryData, urlTuple)
import Util.Time (parseDate)
import Util.String (toSnake)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Time.Calendar (Day)
import Data.Text (Text)

type Year = String

data Author = Author
    { bio :: Maybe Text
    , name :: String
    , links :: Maybe [Url] 
    , personalName :: Maybe String
    , deathDate :: Maybe Year
    , alternateNames :: Maybe [String]
    , created :: Maybe DateTimeObj
    , lastModified :: DateTimeObj
    , latestRevision :: Maybe Int
    , key :: String
    , birthDate :: Maybe Year
    , revision :: Int
    } deriving (Show)

type AuthorUrlQueryData =
    ( String
    , Int
    )

type AuthorQueryData =
    ( Maybe Text
    , String
    , Maybe String
    , Maybe Day
    , Maybe Day
    , Maybe Day
    , Maybe Int
    , String
    , Maybe Day
    , Int
    , Int
    )

urlTuples :: Author -> Maybe [UrlQueryData]
urlTuples (Author { links }) = map urlTuple <$> links 

authorTuple :: Author -> AuthorQueryData
authorTuple (Author { bio, name, personalName, deathDate, created, lastModified, latestRevision, key, birthDate, revision } ) 
    = ( bio
      , name
      , personalName
      , deathDate >>= parseDate
      , created >>= DateTimeObj.value >>= parseDate
      , DateTimeObj.value lastModified >>= parseDate
      , latestRevision
      , key
      , birthDate >>= parseDate
      , revision
      , 0x0 )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Author )


