{-# LANGUAGE UnicodeSyntax #-}
module Test.MySQLModel (run) where

import           Control.Monad              (forM_)
import           Data.Aeson                 (decode)
import           Data.ByteString.Lazy.Char8 (ByteString)
import           Data.Maybe                 (catMaybes)
import           Model.Author
import           Text.Pretty.Simple         (pPrint)

run ∷ [ByteString] → IO()
run jsonObjs = do
    let authors = take 2 . catMaybes $ map decode jsonObjs ∷ [Author]

    forM_ authors $ \author → do
        pPrint author

        putStrLn "\n"
