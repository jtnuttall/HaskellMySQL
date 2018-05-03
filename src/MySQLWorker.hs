{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE LambdaCase        #-}
module MySQLWorker (doMySQLWork) where

import Model.Author as Author
import Model.Book as Book
import Model.Url as Url
import Model.AuthorRef as AuthorRef
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
import qualified Data.ByteString.Lazy as BL
import Data.Int
import System.IO
import qualified Data.ByteString as B
import qualified Data.Set as Set
import Text.Pretty.Simple
import qualified Test.DataModel
import Util.Sanitizer
import Control.Concurrent.Async


insertAuthors :: Connection -> [Author] -> IO Bool
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
                    (\e -> return (e :: SomeException) >>= print >> return 0)


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

    return True


insertBooks :: Connection -> [Book] -> IO Bool
insertBooks conn books = do
    forM_ books $ \book@(Book { Book.title, Book.authors, Book.subjects }) -> do
        putStrLn $ "Working on book " ++ title

        affected <- catch 
            (MySQL.execute conn 
                "INSERT INTO `books` (`title`, `subtitle`, `created`, `description`, `key`) VALUES (?, ?, ?, ?, ?)"
                (bookTuple book)
            )
            (\e -> return (e :: SomeException) >>= print >> return 0)

        if affected <= 0 then
            putStrLn "Insertion failed."
        else do
            putStrLn "Insertion successful."

            putStrLn "Adding subjects..."

            bookid <- MySQL.insertID conn
            when (isJust subjects) $ do
                let subs = fromJust subjects
                affected <- fmap sum . forM subs $ \subject -> do
                    affectedFirst <- MySQL.execute conn
                        "INSERT IGNORE INTO `subjects` (`name`) VALUES (?)"
                        (Only subject)

                    subjectid <- MySQL.insertID conn
                    affectedSecond <- MySQL.execute conn
                        "INSERT IGNORE INTO `books_subjects` (`bookid`, `subjectid`) VALUES (?, ?)"
                        (bookid, subjectid)

                    return (affectedFirst + affectedSecond)

                if affected > 0 then
                    putStrLn "Successfully added subjects."
                else 
                    hPutStrLn stderr "Insertion of subject failed."

            
            insertBookAuthor conn bookid authors

    return True


insertBookAuthor :: Param a => Connection -> a -> [AuthorRef] -> IO()
insertBookAuthor conn bookid authors = do
    affected <- fmap sum . forM authors $ \authorRef -> do
        candidateIds <- MySQL.query conn
            "SELECT (`authorid`) FROM `authors` WHERE `key` LIKE ?"
            (Only (AuthorRef.key <$> (AuthorRef.author $ authorRef)))

        let toInt = (\(Only v) -> v) :: Only Int -> Int

        fmap sum . forM (map toInt candidateIds) $ \authorid -> do
            MySQL.execute conn
                "INSERT IGNORE books_authors (`authorid`, `bookid`) VALUES (?, ?)"
                (authorid, bookid)

    if affected > 0 then
        putStrLn "books_authors init successful"
    else
        putStrLn "failed to initialize books_authors"


doMySQLWork :: [ByteString] -> [ByteString] -> IO()
doMySQLWork authorObjs bookObjsDirty = do
    -- BL.putStrLn (sanitize bookObjs !! 1)
    -- Test.DataModel.run (sanitize bookObjs)
    let bookObjs = sanitize bookObjsDirty

    let authors = take 60000 . drop 600000 . map fromJust . filter isJust $ map decode authorObjs :: [Author]
    let books   = take 100000 . map fromJust . filter isJust $ map decode bookObjs :: [Book]

    conn <- connect 
        defaultConnectInfo
            { connectHost = "303.itpwebdev.com"
            , connectUser = "jtnuttal"
            , connectPassword = "Rain07bow"
            , connectDatabase = "jtnuttal_books_db"
            }

    -- t1 <- forkOS $ insertAuthors conn authors
    -- t2 <- forkOS $ insertBooks conn books

    insertAuthors conn authors
    insertBooks conn books

    -- a1 <- asyncBound (insertAuthors conn authors)
    -- a2 <- asyncBound (insertBooks conn books)
    -- b1 <- wait a1
    -- b2 <- wait a2

    -- print (b1,b2)

    close conn