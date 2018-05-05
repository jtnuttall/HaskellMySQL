{-# LANGUAGE TemplateHaskell #-}
module Model.AuthorRef where

import qualified Model.Author as Author (Author(..))
import Model.DateTimeObj (DateTimeObj(value))
import Data.Aeson.TH
import Util.String (toSnake)
import Data.ByteString.Lazy.Char8 (ByteString, pack)

data KeyType = KeyType 
	{ key :: String }
	deriving (Show)

data AuthorRef = AuthorRef
    { author :: Maybe KeyType } 
    deriving (Show)

refKey :: AuthorRef -> Maybe ByteString
refKey ref = pack <$> key <$> author ref

-- mkAuthor :: AuthorRef -> Maybe Author
-- mkAuthor ref = 
-- 	let mkey = refKey ref 
-- 	in 
-- 		case mkey of 
-- 			Just akey -> 
-- 				Just $
-- 					Author
-- 						{ authorid = Nothing
-- 					    , bio = Nothing
-- 					    , name = ""
-- 					    , links = Nothing
-- 					    , personalName = Nothing
-- 					    , deathDate = Nothing
-- 					    , alternateNames = Nothing
-- 					    , created = Nothing
-- 					    , lastModified = DateTimeObj { value = Nothing }
-- 					    , latestRevision = Nothing
-- 					    , key = akey
-- 					    , birthDate = Nothing
-- 					    , revision = 0
-- 					    }
-- 			Nothing -> Nothing

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''KeyType )
$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''AuthorRef )