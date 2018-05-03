module Test.MySQLModel (run) where

import Util.MySQLValue
import Data.ByteString.Lazy.Char8 (ByteString)

run :: [ByteString] -> IO()
run jsonObjs = do
    let authors = take 2 . map fromJust . filter isJust $ map decode jsonObjs :: [Author]

    forM_ authors $ \author -> do
        pPrint 
            [ toMySqlValue $ key author
            , valueOrNull (bio author)
            , toMySqlValue $ name author
            , valueOrNull (personalName author)
            , valueOrNull (deathDate author)
            , valueOrNull $ created author >>= date >>= parseDate
            , toMySqlValue $ parseDate <$> date (lastModified author)
            , valueOrNull (latestRevision author)
            , toMySqlValue $ key author
            , valueOrNull (birthDate author)
            , toMySqlValue $ revision author
            ]

        putStrLn "\n"