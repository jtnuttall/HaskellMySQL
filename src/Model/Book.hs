{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Model.Book where

import Model.DateTimeObj (DateTimeObj)
import Model.GenericObj (GenericObj)
import qualified Model.GenericObj as GenericObj
import qualified Model.DateTimeObj as DateTimeObj
import Model.Url (Url)
import Model.AuthorRef (AuthorRef)
import Util.Time (parseDate)
import Util.String (toSnake)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Time.Calendar (Day)
import Data.Text (Text)
import Database.MySQL.Simple.Types

data Book = Book
    { title :: String
    , subtitle :: Maybe String
    , created :: Maybe DateTimeObj
    , lastModified :: Maybe DateTimeObj
    , description :: Maybe GenericObj
    , key :: String 
    , authors :: [AuthorRef]
    , subjects :: Maybe [String]
    , links :: Maybe [Url]
    } deriving (Show)

type BookQueryData =
    ( String
    , Maybe String
    , Maybe Day
    , Maybe Text
    , String 
    )

bookTuple :: Book -> BookQueryData
bookTuple ( Book { title, subtitle, created, description, key } ) 
    = ( title
      , subtitle
      , created >>= DateTimeObj.value >>= parseDate
      , description >>= GenericObj.value
      , key 
      )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Book )

