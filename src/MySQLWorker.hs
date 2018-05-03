{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE LambdaCase        #-}
module MySQLWorker (doMySQLWork) where

import Model.Author as Author
import Model.Book as Book
import Model.Url as Url
import Util.MySQLValue
import Control.Monad
import Control.Exception
import Data.Maybe (fromJust, isJust)
import Data.Aeson (decode)
import Data.IORef
import Data.ByteString.Lazy.Char8 (ByteString)
import Database.MySQL.Base (MySQLError(..))
import Database.MySQL.Simple as MySQL
import Database.MySQL.Simple.Param as MySQL
import Data.ByteString.Lazy.Char8 (ByteString)
import Data.Int
import System.IO
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Text.Pretty.Simple
import qualified Test.DataModel
import Util.Sanitizer

insertAuthors :: Connection -> [Author] -> IO()
insertAuthors conn authors = do
    visitedNames <- newIORef Set.empty

    -- construct authors
    forM_ authors $ \author@(Author { Author.name, Author.links }) -> do
        visited <- Set.member name <$> readIORef visitedNames

        when (not visited) $ do
            modifyIORef' visitedNames (Set.insert name)
            putStrLn $ "Working on author " ++ name

            {- INSERT INTO authors 
                ( authorid
                , bio
                , name
                , personalName
                , deathDate
                , created
                , lastModified
                , latestRevision
                , key
                , birthDate
                , revision
                , permissions
                ) VALUES 
                ( ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ? )
            -}
            affected <- 
                catch 
                    (MySQL.execute conn 
                        "INSERT INTO `authors` (`bio`, `name`, `personalName`, `deathDate`, `created`, `lastModified`, `latestRevision`, `key`, `birthDate`, `revision`, `permissions`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                        (authorTuple author)
                    )
                    (\e -> return (e :: SomeException) >>= \_ -> return 0)


            if affected <= 0 then
                putStrLn "Insertion failed."
            else do
                putStrLn "Insertion successful."

                putStrLn "Adding urls..."

                authorid <- MySQL.insertID conn
                case links of
                    Just links -> do
                        forM_ links $ \(Url { url, Url.title }) -> do
                            affected <- MySQL.execute conn
                                "INSERT IGNORE INTO urls (url, title) VALUES (?, ?)"
                                (url, title)

                            if affected > 0 then do
                                urlid <- MySQL.insertID conn

                                affected <- 
                                    catch
                                        (MySQL.execute conn
                                            "INSERT IGNORE INTO links (authorid, urlid) VALUES (?, ?)"
                                            (authorid, urlid)
                                        )
                                        (\e -> return (e :: SomeException) >>= \_ -> return 0)

                                unless (affected > 0) $ 
                                    hPutStrLn stderr "Insertion of link failed."

                            else hPutStrLn stderr "The insert statement for a URL is broken."

                    Nothing -> return ()


insertBooks :: Connection -> [Book] -> IO()
insertBooks conn books = do
    pPrint (books !! 20)

doMySQLWork :: [ByteString] -> [ByteString] -> IO()
doMySQLWork authorObjs bookObjs = do
    Test.DataModel.run (sanitize bookObjs)
    -- let bookObjsCleaned = sanitize bookObjs

    -- let authors = take 600000 . map fromJust . filter isJust $ map decode authorObjs :: [Author]
    -- let books   = take 6000000 . map fromJust . filter isJust $ map decode bookObjsCleaned :: [Book]

    -- conn <- connect 
    --     defaultConnectInfo
    --         { connectHost = "303.itpwebdev.com"
    --         , connectUser = "jtnuttal"
    --         , connectPassword = "Rain07bow"
    --         , connectDatabase = "jtnuttal_books_db"
    --         }

    -- insertAuthors conn authors
    -- insertBooks conn books

    -- close conn