{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Model.Book where

import Model.Object (Object(Object))
import qualified Model.Object as Object
import Model.Url (Url)
import Model.AuthorRef (AuthorRef)
import Util.Time (parseDate)
import Util.String (toSnake)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Time.Calendar (Day)
import Data.Text (Text)
import Database.MySQL.Simple.Types

data Book = Book
    { bookid :: Maybe Int
    , title :: String
    , subtitle :: Maybe String
    , created :: Maybe (Object String)
    , lastModified :: Maybe (Object String)
    , description :: Maybe (Object Text)
    , key :: String 
    , authors :: [AuthorRef]
    , subjects :: Maybe [String]
    , links :: Maybe [Url]
    } deriving (Show)

instance Eq Book where
    (==) b1 b2 = (bookid b1) == (bookid b2)

instance Ord Book where
    compare b1 b2 = compare (bookid b1) (bookid b2)

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
      , created >>= Object.value >>= parseDate
      , description >>= Object.value
      , key 
      )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Book )

