{-# LANGUAGE OverloadedStrings #-}
module Time (parseDate) where

import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)

parseMaybe :: Text -> Text -> Maybe LocalTime
parseMaybe fmt input = parseTimeM True defaultTimeLocale (T.unpack fmt) (T.unpack input)

-- 2008-09-08T16:19:17.29978
tryParseYear :: Text -> Maybe LocalTime
tryParseYear = parseMaybe "%Y"

tryParseFull :: Text -> Maybe LocalTime
tryParseFull = parseMaybe "%Y-%m-%dT%H:%M:%ES"

parseDate :: Text -> Maybe LocalTime
parseDate str = 
    let
        parsers  = [tryParseYear, tryParseFull]
        successes = filter isJust . map ($ str) $ parsers
    in case successes of
        []    -> Nothing
        (t:_) -> t