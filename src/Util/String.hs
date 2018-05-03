module Util.String where

import Data.Char

toSnake :: String -> String
toSnake "" = ""
toSnake (c:cs)
    | isUpper c = '_' : toLower c : toSnake cs
    | otherwise = c : toSnake cs