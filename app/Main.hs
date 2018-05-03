module Main where

import MySQLWorker
import Data.SemiGroup ((<>))
import Options.Applicative

data Args = Args
    { jsonFile :: FilePath }
    deriving (Show)

jsonFile :: Parser Input
jsonFile = 

args :: Parser Sample
args = Args
    <$> strArgument
        ( metavar "jsonFile"
       <> help "The JSON file to work with." 
        )

opts :: ParserInfo Sample
opts = info (sample <**> helper)
    (  fullDesc 
    <> progDesc "Jeremy's MySQL worker for ITP 303"
    )

main :: IO ()
main = doMySQLWork
