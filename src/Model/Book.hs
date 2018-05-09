{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Model.Book where

import           Data.Aeson.TH               (defaultOptions, deriveJSON,
                                              fieldLabelModifier)
import           Data.Text                   (Text)
import           Data.Time.Calendar          (Day)
import           Database.MySQL.Simple.Types
import           Model.AuthorRef             (AuthorRef)
import           Model.Object                (Object (Object))
import qualified Model.Object                as Object
import           Model.Url                   (Url)
import           Util.String                 (toSnake)
import           Util.Time                   (parseDate)

data Book = Book
    { bookid       :: Maybe Int
    , title        :: String
    , subtitle     :: Maybe String
    , created      :: Maybe (Object String)
    , lastModified :: Maybe (Object String)
    , description  :: Maybe (Object Text)
    , key          :: String
    , authors      :: [AuthorRef]
    , subjects     :: Maybe [String]
    , links        :: Maybe [Url]
    } deriving (Show)

instance Eq Book where
    (==) b1 b2 = bookid b1 == bookid b2

instance Ord Book where
    compare b1 b2 = compare (bookid b1) (bookid b2)

type BookQueryData =
    ( String
    , Maybe String
    , Maybe Day
    , Maybe Text
    , String
    )

bookTuple ∷ Book → BookQueryData
bookTuple Book
    { title
    , subtitle
    , created
    , description
    , key
    }
    = ( title
      , subtitle
      , created >>= Object.value >>= parseDate
      , description >>= Object.value
      , key
      )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Book )

