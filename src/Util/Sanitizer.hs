{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Util.Sanitizer (sanitize) where

import Control.Arrow
import Data.Monoid
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified ByteString.StrictBuilder as B
import Data.ByteString.Search
import Data.Coerce
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA.ByteString
import Data.Array

newFieldRegex :: Regex
newFieldRegex = 
    let result = compile defaultCompOpt defaultExecOpt "\" +\".+\":"
    in case result of
        Right regex -> regex
        Left err    -> error err

utf8String :: String -> B.Builder
utf8String = mconcat . map B.utf8Char

cleanDescription :: ByteString -> ByteString
cleanDescription line
    | BS.null rest || BS.null spacefree = line
    | otherwise = 
        case BS.uncons spacefree of
            (Just ('"',_)) -> B.builderBytes
                ( uptoB
               <> B.utf8Char '{'
               <> B.utf8Char '"'
               <> utf8String "value"
               <> B.utf8Char '"'
               <> B.utf8Char ':'
               <> desc
               <> B.utf8Char '}'
               <> rem
                ) 

            _             -> line

    where
        (upto, rest)  = breakAfter "\"description\":" line

        (desc', rem') = 
            case execute newFieldRegex rest of
                Left err     -> error err
                Right marray ->
                    case marray of
                        Nothing      -> ( rest , "" )
                        (Just match) ->
                            let (i, _) = first fromIntegral $ match ! 0
                            in ( BS.take i rest, BS.drop i rest )

        uptoB         = B.bytes upto
        desc          = B.bytes desc'
        rem           = B.bytes rem'
        spacefree     = BS.filter (/= ' ') rest


scrubDuplicates :: ByteString -> B.Builder
scrubDuplicates (BS.uncons -> Nothing) 
    = mempty
scrubDuplicates (BS.uncons -> Just (c, BS.uncons -> Nothing))
    = B.utf8Char c
scrubDuplicates (BS.uncons -> Just (c1, inner@(BS.uncons -> Just (c2, rest)))) 
    | c1 == '\\' && c2 == '"' 
        = scrubDuplicates rest
    | otherwise 
        = B.utf8Char c1 <> scrubDuplicates inner

sanitize :: ByteString -> ByteString
sanitize = cleanDescription . B.builderBytes . scrubDuplicates

fromJust :: Integral a => a -> Maybe a -> a
fromJust _ (Just x) = x
fromJust x Nothing  = x