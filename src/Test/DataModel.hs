{-# LANGUAGE UnicodeSyntax #-}
module Test.DataModel (run) where

import           Control.Monad              (forM_)
import           Data.Aeson                 (decode, eitherDecode)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Either                (isLeft, isRight, rights)
import           Model.Author
import           Model.Book
import           Text.Pretty.Simple
import           Text.Printf                (printf)

run ∷ [C.ByteString] → IO()
run jsonObjs = do
    let decoded = map eitherDecode jsonObjs :: [Either String Book]

    putStrLn "Here's two of the decoded objects:"
    forM_ (take 2 decoded) $ \obj -> do
        pPrint obj
        putStrLn "\n"

    putStrLn "Were there errors?"
    putStrLn $ if any isLeft decoded then "\tYES" else "\tNO"

    putStrLn "\n"

    putStrLn "The first 20 errors were:"
    forM_ (take 20 . filter isLeft $ decoded) $ \(Left err) ->
        putStrLn $ "\t" ++ err

    putStrLn "\n"

    let parsedCorrectly = rights . filter isRight $ decoded
        total    = length decoded
        total'   = fromIntegral total :: Double
        partial  = length parsedCorrectly
        partial' = fromIntegral partial :: Double

    putStrLn "Stats:"
    printf "We have successfully integrated %d / %d of the books.\n" partial total
    printf "This is %0.2f percent of them.\n" (100 * partial' / total')
