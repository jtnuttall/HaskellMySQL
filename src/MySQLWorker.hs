module MySQLWorker (doMySQLWork) where

import Data.ByteString.Lazy.Char8 (ByteString)

doWork :: [ByteString] -> IO()
doWork jsonObjs =
    testMySqlModel jsonObjs

    -- let decoded = take 600000 . map fromJust . filter isJust $ map decode jsonObjs :: [Author]

    -- conn <- connect 
    --     defaultConnectInfo
    --         { ciHost = "303.itpwebdev.com"
    --         , ciUser = "jtnuttal"
    --         , ciPassword = "Rain07bow"
    --         , ciDatabase = "jtnuttal_books_db"
    --         }

    -- (defs, is) <- query_ conn "SELECT * FROM authors"

    -- let authorsStmt = prepareStmt conn 
    --         "INSERT INTO authors (`authorid`, `bio`, `name`, `personalName`, `deathDate`, `created`, `lastModified`, `latestRevision`, `key`, `birthDate`, `revision`) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?);"

    -- forM_ decoded $ \author ->
    --     execute conn authorsStmt
    --         [ key author :: MySQLValue
    --         , valueOrNull (bio author)
    --         , name author
    --         , valueOrNull (personalName author)
    --         , valueOrNull (deathDate author)
    --         , valueOrNull . parseDate . date $ created author
    --         , parseDate . date $ lastModified author
    --         , valueOrNull (latestRevision author)
    --         , key author
    --         , valueOrNull (birthDate author)
    --         , revision author
    --         ]

    -- close conn