{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MySQLWorker (doMySQLWork) where

import           Control.Arrow
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Monad
import           Data.Aeson                         (decodeStrict)
import qualified Data.ByteString                    as B
import qualified Data.ByteString.Lazy               as BL
import           Data.ByteString.Lazy.Char8         (ByteString)
import           Data.Int                           (Int64)
import qualified Data.IntSet                        as IntSet
import           Data.IORef
import qualified Data.Map                           as Map
import           Data.Maybe                         (catMaybes, fromJust,
                                                     isJust)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.String
import qualified Data.Text.IO                       as T
import qualified Data.Vector                        as V
import qualified Data.Vector.Algorithms.Intro       as MVAlgo
import qualified Data.Vector.Algorithms.Search      as MVSearch
import qualified Data.Vector.Mutable                as MV
import           Database.MySQL.Base                (MySQLError (..))
import           Database.MySQL.Base.Types          (Field (..))
import           Database.MySQL.Simple              as MySQL
import           Database.MySQL.Simple.Param        as MySQL
import           Database.MySQL.Simple.QueryResults as MySQL
import           Database.MySQL.Simple.Result       as MySQL
import           Model.Author                       as Author
import           Model.AuthorRef                    as AuthorRef
import           Model.Book                         as Book
import           Model.Url                          as Url
import           System.IO
import qualified System.IO.Streams                  as Streams
import qualified System.IO.Streams.Combinators      as Streams
import           System.Random
import qualified Test.DataModel
import           Text.Pretty.Simple
import           Text.Printf
import           Util.Sanitizer

connectInfo =
    defaultConnectInfo
        { connectHost = "303.itpwebdev.com"
        , connectUser = "jtnuttal"
        , connectPassword = "Rain07bow"
        , connectDatabase = "jtnuttal_books_db"
        }


insertAuthor ∷ Connection → Bool → Author → Set String → IO (Set String)
insertAuthor conn verbose author@Author { Author.name, Author.links } visitedNames = do
    let visited = Set.member name visitedNames

    if visited then return visitedNames
    else do
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
                (\e -> print (e :: SomeException)  >> return 0)


        if affected <= 0 then do
            when verbose $ putStrLn "Insertion failed."

            return visitedNames
        else do
            when verbose $ do
                putStrLn "Insertion successful."
                putStrLn "Adding urls..."

            authorid <- MySQL.insertID conn
            case links of
                Just links -> do
                    forM_ links $ \Url { Url.url, Url.title } -> do
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
                                    (\e -> (\_ -> return 0) (e :: SomeException))


                            when verbose $
                                unless (affected > 0) $
                                    putStrLn "Insertion of link failed."

                        else
                            when verbose $
                                putStrLn "The insert statement for a URL is broken."

                    return (Set.insert name visitedNames)

                Nothing -> return (Set.insert name visitedNames)

        return (Set.insert name visitedNames)

    return visitedNames


insertBook ∷ Connection → Bool → Book → IO ()
insertBook conn verbose book@Book { Book.title, Book.authors, Book.subjects } = do
    putStrLn $ "Working on book " ++ title

    affected <- catch
        (MySQL.execute conn
            "INSERT INTO `books` (`title`, `subtitle`, `created`, `description`, `key`) VALUES (?, ?, ?, ?, ?)"
            (bookTuple book)
        )
        (\e -> print (e :: SomeException) >> return 0)

    if affected <= 0 then
        when verbose $
            putStrLn "Insertion failed."
    else do
        when verbose $ do
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

            when verbose $
                if affected > 0 then
                    putStrLn "Successfully added subjects."
                else
                    putStrLn "Insertion of subject failed."


        insertBookAuthor conn verbose bookid authors

insertBookAuthor ∷ Param a ⇒ Connection → Bool → a → [AuthorRef] → IO()
insertBookAuthor conn verbose bookid authors = do
    affected <- fmap sum . forM authors $ \authorRef -> do
        candidateIds <- MySQL.query conn
            "SELECT (`authorid`) FROM `authors` WHERE `key` LIKE ?"
            (Only (AuthorRef.key <$> AuthorRef.author authorRef))

        let toInt = (\(Only v) -> v) :: Only Int → Int

        fmap sum . forM (map toInt candidateIds) $ \authorid ->
            MySQL.execute conn
                "INSERT IGNORE books_authors (`authorid`, `bookid`) VALUES (?, ?)"
                (authorid, bookid)

    when verbose $
        if affected > 0 then
            putStrLn "books_authors init success"
        else
            putStrLn "books_authors init failure"

type Stream = Streams.InputStream B.ByteString

doMySQLWork ∷ Stream → Int64 → Int64 → Stream → Int64 → Int64 → Bool → IO()
doMySQLWork authorsStream_ numAuthors authorStart booksStream_ numBooks bookStart verbose = do
    conn <- MySQL.connect connectInfo

    when verbose $ putStrLn "Operating on authors..."

    authorsStream <-
        Streams.drop authorStart authorsStream_ >>=
            Streams.take numAuthors >>=
                Streams.map decodeStrict >>=
                    Streams.filter isJust >>=
                        Streams.map fromJust

    Streams.foldM_
        (flip $ insertAuthor conn verbose)
        (return Set.empty)
        return
        authorsStream

    when verbose $ putStrLn "Operating on books..."

    booksStream <-
        Streams.drop bookStart booksStream_ >>=
            Streams.take numBooks >>=
                Streams.map (decodeStrict . sanitize) >>=
                    Streams.filter isJust >>=
                        Streams.map fromJust

    Streams.mapM_
        (insertBook conn verbose)
        booksStream

    MySQL.close conn
