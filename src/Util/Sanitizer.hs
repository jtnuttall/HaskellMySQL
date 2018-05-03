{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}
module Util.Sanitizer (sanitize) where

import Control.Arrow
import Data.Monoid
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy.Search
import Data.Coerce
import Text.Regex.Base.RegexLike
import Text.Regex.TDFA.ByteString.Lazy
import Debug.Trace
import Data.Array

newFieldRegex :: Regex
newFieldRegex = 
	let result = compile defaultCompOpt defaultExecOpt "\" +\".+\":"
	in case result of
		Right regex -> regex
		Left err    -> error err

cleanDescriptions :: [ByteString] -> [ByteString]
cleanDescriptions [] = []
cleanDescriptions (line:s)
	| BS.null rest || BS.null spacefree = line : sanitize s
	| otherwise = 
		case (BS.uncons spacefree) of
			(Just ('"',_)) -> trace ("build") B.toLazyByteString
				( uptoB
			   <> B.charUtf8   '{'
			   <> B.charUtf8   '"'
			   <> B.stringUtf8 "value"
			   <> B.charUtf8   '"'
			   <> B.charUtf8   ':'
			   <> desc
			   <> B.charUtf8   '}'
			   <> rem
			    ) : sanitize s
			_          -> line : sanitize s

	where
		(upto, rest)  = breakAfter "\"description\":" line

		(desc', rem') = 
			case execute newFieldRegex rest of
				Left err -> error err
				Right marray ->
					case marray of
						Nothing      -> ( rest , "" )
						(Just match) ->
							let (i, _) = (first fromIntegral) $ match ! 0
							in ( BS.take i rest, BS.drop i rest )

		-- (desc', rem') = BS.splitAt (fromJust (BS.length rest) $ BS.elemIndex '"' rest) rest
		uptoB         = B.lazyByteString upto
		desc          = B.lazyByteString desc'
		rem           = B.lazyByteString rem'
		spacefree     = BS.filter (/= ' ') rest


scrubDuplicates :: [ByteString] -> [B.Builder]
scrubDuplicates [] = []
scrubDuplicates (line:s) = scrub line : scrubDuplicates s
	where
		scrub (BS.uncons -> Nothing) 
			= mempty
		scrub (BS.uncons -> Just (c, (BS.uncons -> Nothing)))
			= B.charUtf8 c
		scrub (BS.uncons -> Just (c1, inner@(BS.uncons -> Just (c2, rest)))) 
			| c1 == '\\' && c2 == '"' 
				= scrub rest
			| otherwise 
				= B.charUtf8 c1 <> scrub inner

sanitize :: [ByteString] -> [ByteString]
sanitize = cleanDescriptions . map B.toLazyByteString . scrubDuplicates

fromJust :: Integral a => a -> Maybe a -> a
fromJust _ (Just x) = x
fromJust x Nothing  = x