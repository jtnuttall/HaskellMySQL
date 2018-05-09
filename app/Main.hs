{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Data.Int            (Int64)
import           Data.Semigroup      ((<>))
import           MySQLWorker         (doMySQLWork)
import           Options.Applicative
import           System.IO           (IOMode (ReadMode), withBinaryFile)
import qualified System.IO.Streams   as Streams

data Args = Args
    { authorsJSON :: FilePath
    , numAuthors  :: Int64
    , startAuthor :: Int64
    , booksJSON   :: FilePath
    , numBooks    :: Int64
    , startBook   :: Int64
    , verbose     :: Bool
    } deriving (Show)

argsParser :: Parser Args
argsParser = Args
    <$> strArgument
        ( metavar "AUTHORS"
       <> help "The authors json file to work with."
        )
    <*> option auto
        ( metavar "NUMAUTHORS"
       <> long "num-authors"
       <> short 'a'
       <> help "The number of authors to process. Defaults to all."
       <> value (negate 1)
        )
    <*> option auto
        ( metavar "STARTAUTHOR"
       <> long "start-author"
       <> help "The index of the author to start with. Defaults to 0."
       <> value 0
        )
    <*> strArgument
        ( metavar "BOOKS"
       <> help "The books json to work with."
        )
    <*> option auto
        ( metavar "NUMBOOKS"
       <> long "num-books"
       <> short 'b'
       <> help "The number of books to process. Defaults to all."
       <> value (negate 1)
        )
    <*> option auto
        ( metavar "STARTBOOK"
       <> long "start-book"
       <> help "The index of the book to start with. Defaults to 0."
       <> value 0
        )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Print interstitial success or failure messages."
        )

opts :: ParserInfo Args
opts = info (argsParser <**> helper)
    (  fullDesc
    <> progDesc "Jeremy's MySQL worker for ITP 303"
    )

main :: IO ()
main = do
    args <- execParser opts

    withBinaryFile (authorsJSON args) ReadMode $ \authorsHandle ->
        withBinaryFile (booksJSON args) ReadMode $ \booksHandle -> do
            authorsStream <- Streams.handleToInputStream authorsHandle >>= Streams.lines
            booksStream   <- Streams.handleToInputStream booksHandle >>= Streams.lines

            doMySQLWork
                authorsStream
                (numAuthors args)
                (startAuthor args)
                booksStream
                (numBooks args)
                (startBook args)
                (verbose args)
