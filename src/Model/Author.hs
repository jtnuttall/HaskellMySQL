{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Model.Author where

import Model.Object (Object(Object))
import qualified Model.Object as Object
import Model.Url (Url, UrlQueryData, urlTuple)
import Util.Time (parseDate)
import Util.String (toSnake)
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Time.Calendar (Day)
import Data.Text (Text)

data Author = Author
    { authorid :: Maybe Int
    , bio :: Maybe Text
    , name :: String
    , links :: Maybe [Url] 
    , personalName :: Maybe String
    , deathDate :: Maybe String
    , alternateNames :: Maybe [String]
    , created :: Maybe (Object String)
    , lastModified :: Object String
    , latestRevision :: Maybe Int
    , key :: String
    , birthDate :: Maybe String
    , revision :: Int
    } deriving (Show)


instance Eq Author where
    (==) a1 a2 = authorid a1 == authorid a2

instance Ord Author where 
    compare a1 a2 = compare (authorid a1) (authorid a2)

mkDummyAuthor :: Int -> Author
mkDummyAuthor authorid_ =
    Author
        { authorid = Just authorid_
        , bio = Nothing
        , name = ""
        , links = Nothing
        , personalName = Nothing
        , deathDate = Nothing
        , alternateNames = Nothing
        , created = Nothing
        , lastModified = Object { Object.value = Just "" }
        , latestRevision = Nothing
        , key = ""
        , birthDate = Nothing
        , revision = 0
        }

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
      , created >>= Object.value >>= parseDate
      , Object.value lastModified >>= parseDate
      , latestRevision
      , key
      , birthDate >>= parseDate
      , revision
      , 0x0 )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Author )


