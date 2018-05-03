{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NamedFieldPuns  #-}
module Model.Url where

import Util.String (toSnake)
import Data.Aeson.TH

data Url = Url
    { url :: String
    , title :: String
    } deriving (Show)


type UrlQueryData =
    ( String
    , String
    )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Url )


urlTuple :: Url -> UrlQueryData
urlTuple (Url { url, title }) = (url, title)
