{-# LANGUAGE NamedFieldPuns #-}
module Main where

import MySQLWorker
import Data.Semigroup ((<>))
import Options.Applicative
import qualified Data.ByteString.Lazy.Char8 as C

data Args = Args
    { authorsJSON :: FilePath
    , booksJSON :: FilePath
	} deriving (Show)

args :: Parser Args
args = Args
    <$> strArgument
        ( metavar "AUTHORS"
       <> help "The authors json file to work with." 
        )
    <*> strArgument
    	( metavar "BOOKS"
       <> help "The books json to work with."
        )

opts :: ParserInfo Args
opts = info (args <**> helper)
    (  fullDesc 
    <> progDesc "Jeremy's MySQL worker for ITP 303"
    )

main :: IO ()
main = do 
	args <- execParser opts 

	authorsRaw <- C.lines <$> C.readFile (authorsJSON args)
	booksRaw   <- C.lines <$> C.readFile (booksJSON args)

	doMySQLWork authorsRaw booksRaw
