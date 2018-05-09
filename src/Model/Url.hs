{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}
module Model.Url where

import           Data.Aeson.TH
import           Util.String   (toSnake)

data Url = Url
    { url   :: String
    , title :: String
    } deriving (Show)


type UrlQueryData =
    ( String
    , String
    )

$( deriveJSON (defaultOptions { fieldLabelModifier = toSnake }) ''Url )


urlTuple ∷ Url → UrlQueryData
urlTuple Url { url, title } = (url, title)
