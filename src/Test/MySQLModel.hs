module Test.MySQLModel (run) where

import Model.Author
import Util.MySQLValue
import Control.Monad (forM_)
import Data.Maybe (fromJust, isJust)
import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy.Char8 (ByteString)

run :: [ByteString] -> IO()
run jsonObjs = do
    let authors = take 2 . map fromJust . filter isJust $ map decode jsonObjs :: [Author]

    forM_ authors $ \author -> do
        pPrint author

        putStrLn "\n"