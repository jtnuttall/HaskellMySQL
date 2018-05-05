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
import Control.Arrow
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.Aeson (decode)
import Data.IORef
import Data.ByteString.Lazy.Char8 (ByteString)
import Database.MySQL.Base (MySQLError(..))
import Database.MySQL.Simple as MySQL
import Database.MySQL.Simple.Param as MySQL
import Database.MySQL.Simple.Result as MySQL
import Database.MySQL.Simple.QueryResults as MySQL
import Database.MySQL.Base.Types (Field(..))
import qualified Data.ByteString.Lazy as BL
import Data.Int
import System.IO
import qualified Data.ByteString as B
import qualified Data.Set as Set
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import Text.Pretty.Simple
import qualified Test.DataModel
import Util.Sanitizer
import Control.Concurrent.Async
import System.Random
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Algorithms.Intro as MVAlgo
import qualified Data.Vector.Algorithms.Search as MVSearch
import qualified Data.Text.IO as T
import Text.Printf

insertAuthors :: Connection -> [Author] -> IO Bool
insertAuthors conn authors = do
    visitedNames <- newIORef Set.empty

    -- construct authors
    forM_ authors $ \author@(Author { Author.name, Author.links }) -> do
        visited <- Set.member name <$> readIORef visitedNames

        unless visited $ do
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
                    (\e -> print (e :: SomeException)  >> return 0)


            if affected <= 0 then
                putStrLn "Insertion failed."
            else do
                putStrLn "Insertion successful."

                putStrLn "Adding urls..."

                authorid <- MySQL.insertID conn
                case links of
                    Just links ->
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
                                        ((\_ -> return 0) (e :: SomeException))

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
            (\e -> print (e :: SomeException) >> return 0)

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


reduce :: QueryResults r => [[r]] -> [r]
reduce = concat

insertBookAuthor :: Param a => Connection -> a -> [AuthorRef] -> IO()
insertBookAuthor conn bookid authors = do
    affected <- fmap sum . forM authors $ \authorRef -> do
        candidateIds <- MySQL.query conn
            "SELECT (`authorid`) FROM `authors` WHERE `key` LIKE ?"
            (Only (AuthorRef.key <$> (AuthorRef.author $ authorRef)))

        let toInt = (\(Only v) -> v) :: Only Int -> Int

        fmap sum . forM (map toInt candidateIds) $ \authorid -> 
            MySQL.execute conn
                "INSERT IGNORE books_authors (`authorid`, `bookid`) VALUES (?, ?)"
                (authorid, bookid)

    if affected > 0 then
        putStrLn "books_authors init successful"
    else
        putStrLn "failed to initialize books_authors"

insertBookAuthorConn :: Connection -> [Author] -> [Book] -> IO ()
insertBookAuthorConn conn authors_ books_ = do
    authorsVec <- V.unsafeThaw . V.fromList $ authors_
    booksVec   <- V.unsafeThaw . V.fromList $ books_

    forM_ (zip authors_ [0..]) $ \( Author { Author.key }, i ) -> do
            authorids <- MySQL.query conn
                "SELECT `authorid` FROM `authors` WHERE `key` LIKE ?"
                (Only key)

            if null authorids then do
                putStrLn "Failed to get authorid."
                q <- MySQL.formatQuery conn
                    "SELECT `authorid` FROM `authors` WHERE `key` LIKE ?"
                    (Only key)
                pPrint q
            else 
                forM_ authorids $ \(Only aid) ->
                    MV.modify authorsVec (\author -> author { Author.authorid = Just aid }) i

    -- authorsSorted <- MV.new (MV.length authorsVec) :: IO (MV.IOVector Author)
    -- MV.unsafeCopy authorsSorted authorsVec
    -- MVAlgo.sort authorsSorted

    forM_ (zip books_ [0..]) $ \( Book { Book.key }, i ) -> do
        bookids <- MySQL.query conn
            "SELECT `bookid` FROM `books` WHERE `key` LIKE ?"
            (Only key)

        if null bookids then putStrLn "Failed to get bookid."
        else
            forM_ bookids $ \(Only bid) ->
                MV.modify booksVec (\book -> book { Book.bookid = Just bid }) i

    books <- V.unsafeFreeze booksVec

    replicateM_ 6000000 $ do
        authorix <- randomRIO (0, MV.length authorsVec - 1)
        bookix   <- randomRIO (0, MV.length booksVec - 1)
        putStrLn "Inserting random author, book pair."

        authorid <- Author.authorid <$> MV.read authorsVec authorix
        bookid   <- Book.bookid <$> MV.read booksVec bookix

        when (isJust authorid && isJust bookid) $ do
            affected <- MySQL.execute conn
                "INSERT IGNORE INTO `books_authors` (`bookid`, `authorid`) VALUES (?, ?)"
                (authorid, bookid)

            putStrLn $ if affected > 0 then "Success" else "Failure"
    -- forM_ books $ \( Book { Book.bookid, Book.title, Book.authors = as } ) ->
    --     case bookid of
    --         Nothing      -> do
    --             printf "ERROR: No bookid for book '%s'" title
    --             return ()
    --         Just bookid' -> do
    --             -- refKey :: AuthorRef -> ByteString
    --             -- as :: [AuthorRef]
    --             let refKeys = catMaybes . map refKey $ as

    --             when (null refKeys) $ putStrLn "NULL refs"

    --             authorResults <- forM refKeys $ \rkey ->
    --                 MySQL.query conn
    --                     "SELECT `authorid` FROM `authors` WHERE `key` LIKE ?"
    --                     (Only rkey)

    --             when (null authorResults) $ putStrLn "NULL author results"

    --             indices <- forM (reduce authorResults) $ \(Only authorid) -> 
    --                 MVSearch.binarySearch authorsSorted (mkDummyAuthor authorid)

    --             forM_ indices $ \i -> do
    --                 when (i < MV.length authorsSorted) $ do
    --                     (Author { Author.authorid }) <- MV.read authorsSorted i

    --                     affected <- MySQL.execute conn
    --                         "INSERT IGNORE `books_authors` (`bookid`, `authorid`) VALUES (?, ?)"
    --                         (bookid', authorid)

    --                     unless (affected > 0) $ putStrLn "Insertion into books_authors failed."

    -- let authors = Map.fromList . map (authorid &&& id) $ authors_
    --     -- map (Book.authors &&& id) books :: [([Author], [Book])]
    --     books   = Map.fromList . map (Book.authors &&& replicate (length authors) . id) $ books_
    -- in do
    --     putStrLn "Sets ready."

-- insertUsers :: Connection -> [Author] -> IO()
-- insertUsers conn a_ = let auths =  V.fromList in do
--     rands <- replicateM 2000 $ randomRIO (0, V.length auths - 1)

--     T.writeFile "Files/Users.out" $ pShow ("username", "email", "password", "author page")

--     visitedIndices <- newIORef IntSet.empty
--     forM_ rands $ \r -> do
--         visitedThisIndex <- IntSet.member r <$> readIORef visitedIndices
--         unless visitedThisIndex $ do
--             let ( Author { Author.name, Author.key } ) = auths V.! r
--                 emails = [ "gmail.com", "hotmail.com", "yahoo.com", "usc.edu"]

--             r' <- randomRIO (0, length emails)

--             let user = 
--                     ( name ++ "_" ++ show r  --username
--                     , name "@" (emails V.! r') --email
--                     , show (r * r')          --password
--                     , name                   --realname
--                     )

--             altered <- 
--                 (execute conn 
--                     "INSERT INTO `users` ( username, email, hash, realname ) VALUES (?, ?, ?, ?)"
--                     user)
--                     `catch`
--                         (\e -> return (e :: SomeException) >>= print >> return 0)

--             unless (altered > 0) $ hPutStrLn stderr "Failure to update user table."

--             T.appendFile "Files/Users.out" $ pShow user

connectInfo = 
    defaultConnectInfo
        { connectHost = "303.itpwebdev.com"
        , connectUser = "jtnuttal"
        , connectPassword = "Rain07bow"
        , connectDatabase = "jtnuttal_books_db"
        }

doMySQLWork :: [ByteString] -> [ByteString] -> IO()
doMySQLWork authorObjs bookObjsDirty = do
    -- BL.putStrLn (sanitize bookObjs !! 1)
    -- Test.DataModel.run (sanitize bookObjs)
    -- let bookObjs = sanitize bookObjsDirty

    putStrLn "Reading authors...this may take awhile."

    let authors = catMaybes $ map decode authorObjs :: [Author]
        books   = take 600000 . catMaybes $ map decode bookObjsDirty :: [Book]

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

    -- insertBookAuthorConn conn authors books

    -- insertUsers conn authors

    -- a1 <- asyncBound (insertAuthors conn authors)
    -- a2 <- asyncBound (insertBooks conn books)
    -- b1 <- wait a1
    -- b2 <- wait a2

    -- print (b1,b2)

    close conn