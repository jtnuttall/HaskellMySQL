{-# LANGUAGE OverloadedStrings #-}
module Util.Time (parseDate) where

import Data.Maybe (isJust)
import Data.Time.Calendar (Day)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

parseMaybe :: String -> String -> Maybe Day
parseMaybe = parseTimeM True defaultTimeLocale

-- 2008-09-08T16:19:17.29978
tryParseYear :: String -> Maybe Day
tryParseYear = parseMaybe "%Y"

tryParseDots :: String -> Maybe Day
tryParseDots = parseMaybe "%d.%m.%Y"

tryParseFull :: String -> Maybe Day
tryParseFull = parseMaybe "%Y-%m-%dT%H:%M:%ES"

parseDate :: String -> Maybe Day
parseDate str = 
    let
        parsers  = [tryParseYear, tryParseDots, tryParseFull]
        successes = filter isJust . map ($ str) $ parsers
    in case successes of
        []    -> Nothing
        (t:_) -> t