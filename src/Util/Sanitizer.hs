{-# LANGUAGE OverloadedStrings #-}
module Util.Sanitizer (sanitize) where

import Control.Arrow
import Data.Semigroup
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Builder as B
import Data.ByteString.Lazy.Search
import Data.Coerce
import Debug.Trace

sanitize :: [ByteString] -> [ByteString]
sanitize [] = []
sanitize (line:s)
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
		(desc', rem') = BS.splitAt (fromJust (BS.length rest) $ BS.elemIndex '"' rest) rest
		uptoB         = B.lazyByteString upto
		desc          = B.lazyByteString desc'
		rem           = B.lazyByteString rem'
		spacefree     = BS.filter (/= ' ') rest

fromJust :: Integral a => a -> Maybe a -> a
fromJust _ (Just x) = x
fromJust x Nothing  = x