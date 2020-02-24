{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module MySQLWorker (doMySQLWork) where

import           Control.DeepSeq                    (rnf)
import           Control.Exception                  (SomeException, catch, evaluate)
import           Control.Monad                      (forM, forM_, when, unless, void)
import           Data.Aeson                         (decodeStrict)
import           Data.ByteString                    (ByteString)
import           Data.Int                           (Int64)
import           Data.Maybe                         (fromJust, isJust)
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Database.MySQL.Base                (MySQLError (..))
import           Database.MySQL.Base.Types          (Field (..))
import           Database.MySQL.Simple              as MySQL
import           Database.MySQL.Simple.Param        as MySQL
import           Database.MySQL.Simple.QueryResults as MySQL
import           Database.MySQL.Simple.Result       as MySQL
import           GHC.Conc                           (atomically)
import           Model.Author                       as Author
import           Model.AuthorRef                    as AuthorRef
import           Model.Book                         as Book
import           Model.Url                          as Url
import qualified Pipes.Concurrent                   as Pipes
import qualified System.IO.Streams                  as Streams
import qualified System.IO.Streams.Combinators      as Streams
import           Text.Pretty.Simple                 (pPrint)
import           Util.Sanitizer                     (sanitize)

type Out = Pipes.Output String

connectInfo =
    defaultConnectInfo
        { connectHost = ""
        , connectUser = ""
        , connectPassword = ""
        , connectDatabase = ""
        }


insertAuthor ∷ Connection → Bool → Out → Author → Set String → IO (Set String)
insertAuthor conn verbose outPipe author@Author { Author.name, Author.links } visitedNames = do
    let visited = Set.member name visitedNames

    if visited then return visitedNames
    else do
        void . atomically . Pipes.send outPipe $ "Working on author " ++ name

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
        affected ←
            catch
                (MySQL.execute conn
                    "INSERT INTO `authors` (`bio`, `name`, `personalName`, `deathDate`, `created`, `lastModified`, `latestRevision`, `key`, `birthDate`, `revision`, `permissions`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
                    (authorTuple author)
                )
                (\e → print (e :: SomeException)  >> return 0)


        if affected <= 0 then do
            when verbose $ putStrLn "Insertion failed."

            return visitedNames
        else do
            when verbose . void . atomically $ do
                Pipes.send outPipe "Insertion successful."
                Pipes.send outPipe "Adding urls..."

            authorid ← MySQL.insertID conn
            case links of
                Just links → do
                    forM_ links $ \Url { Url.url, Url.title } → do
                        affected ← MySQL.execute conn
                            "INSERT IGNORE INTO urls (url, title) VALUES (?, ?)"
                            (url, title)

                        if affected > 0 then do
                            urlid ← MySQL.insertID conn

                            affected ←
                                catch
                                    (MySQL.execute conn
                                        "INSERT IGNORE INTO links (authorid, urlid) VALUES (?, ?)"
                                        (authorid, urlid)
                                    )
                                    (\e → (const $ return 0) (e :: SomeException))


                            when verbose $
                                unless (affected > 0) . void . atomically $
                                    Pipes.send outPipe "Insertion of link failed."

                        else
                            when verbose . void . atomically $
                                Pipes.send outPipe "The insert statement for a URL is broken."

                    return (Set.insert name visitedNames)

                Nothing → return (Set.insert name visitedNames)

        return (Set.insert name visitedNames)

    return visitedNames


insertBook ∷ Connection → Bool → Out → Book → Bool → IO Bool
insertBook conn verbose outPipe book@Book { Book.title, Book.authors, Book.subjects } _ = do
    void . atomically . Pipes.send outPipe $ "Working on book " ++ title

    affected ← catch
        (MySQL.execute conn
            "INSERT INTO `books` (`title`, `subtitle`, `created`, `description`, `key`) VALUES (?, ?, ?, ?, ?)"
            (bookTuple book)
        )
        (\e → print (e :: SomeException) >> return 0)

    if affected <= 0 then do
        when verbose . void . atomically $
            Pipes.send outPipe "Insertion failed."

        return False
    else do
        when verbose . void . atomically $ do
            Pipes.send outPipe "Insertion successful."
            Pipes.send outPipe "Adding subjects..."

        bookid ← MySQL.insertID conn
        when (isJust subjects) $ do
            let subs = fromJust subjects
            affected ← fmap sum . forM subs $ \subject → do
                affectedFirst ← MySQL.execute conn
                    "INSERT IGNORE INTO `subjects` (`name`) VALUES (?)"
                    (Only subject)

                subjectid ← MySQL.insertID conn
                affectedSecond ← MySQL.execute conn
                    "INSERT IGNORE INTO `books_subjects` (`bookid`, `subjectid`) VALUES (?, ?)"
                    (bookid, subjectid)

                return (affectedFirst + affectedSecond)

            when verbose . void . atomically $
                if affected > 0 then
                    Pipes.send outPipe "Successfully added subjects."
                else
                    Pipes.send outPipe "Insertion of subject failed."


        insertBookAuthor conn verbose outPipe bookid authors
        return True

insertBookAuthor ∷ Param a ⇒ Connection → Bool → Out → a → [AuthorRef] → IO()
insertBookAuthor conn verbose outPipe bookid authors = do
    affected ← fmap sum . forM authors $ \authorRef → do
        candidateIds ← MySQL.query conn
            "SELECT (`authorid`) FROM `authors` WHERE `key` LIKE ?"
            (Only (AuthorRef.key <$> AuthorRef.author authorRef))

        let toInt = (\(Only v) → v) :: Only Int → Int

        fmap sum . forM (map toInt candidateIds) $ \authorid →
            MySQL.execute conn
                "INSERT IGNORE books_authors (`authorid`, `bookid`) VALUES (?, ?)"
                (authorid, bookid)

    when verbose . void . atomically $
        if affected > 0 then
            Pipes.send outPipe "books_authors init success"
        else
            Pipes.send outPipe "books_authors init failure"

type Stream = Streams.InputStream ByteString

doMySQLWork 
    ∷ Stream 
    → Int64 
    → Int64 
    → Stream 
    → Int64 
    → Int64 
    → Bool 
    → Out
    → IO()
doMySQLWork authorsStream_ numAuthors authorStart booksStream_ numBooks bookStart verbose outPipe = do
    let takeAuthors = if numAuthors < 0 then return . id else Streams.take numAuthors
        takeBooks   = if numBooks < 0 then return . id else Streams.take numBooks

    conn ← MySQL.connect connectInfo


    void . atomically $ Pipes.send outPipe "Operating on authors..."
    authorsStream ←
        Streams.drop authorStart authorsStream_ >>=
            takeAuthors >>=
                Streams.map decodeStrict >>=
                    Streams.filter isJust >>=
                        Streams.map fromJust


    {- Use a fold to eliminate the need for an IORef to the set. -}
    Streams.foldM_
        (flip $ insertAuthor conn verbose outPipe)
        (return Set.empty)
        return
        authorsStream


    void . atomically $ Pipes.send outPipe "Operating on books..."
    booksStream ←
        Streams.drop bookStart booksStream_ >>=
            takeBooks >>=
                Streams.map (decodeStrict . sanitize) >>=
                    Streams.filter isJust >>=
                        Streams.map fromJust

    {- Evaluation of I/O is not happening properly without using
       a similar fold.
     -}
    Streams.foldM_
        (flip $ insertBook conn verbose outPipe)
        (return True)
        return
        booksStream

    MySQL.close conn
