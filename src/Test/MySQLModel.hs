module Test.MySQLModel (run) where

import Model.Author
import Control.Monad (forM_)
import Data.Maybe (catMaybes)
import Data.Aeson (decode)
import Text.Pretty.Simple (pPrint)
import Data.ByteString.Lazy.Char8 (ByteString)

run :: [ByteString] -> IO()
run jsonObjs = do
    let authors = take 2 . catMaybes $ map decode jsonObjs :: [Author]

    forM_ authors $ \author -> do
        pPrint author

        putStrLn "\n"