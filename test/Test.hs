{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Handsy              as H
import           System.Handsy.Remote       as H

import           Control.Applicative
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import           Data.List
import           System.Exit

import           Test.Tasty
import           Test.Tasty.HUnit

arbitraryBinary :: B.ByteString
arbitraryBinary = B.pack [1..255]

createTempFile :: Handsy String
createTempFile = dropWhileEnd isSpace . C.unpack . fst <$> command_ "mktemp" [] ""

test1 = testCase "writeFile . readFile == id" $ do
  ret <- H.run options{debug=True} $ do
    tmp <- createTempFile

    H.writeFile tmp arbitraryBinary
    H.readFile tmp

  assertEqual "" arbitraryBinary ret

test2 = testCase "appendFile" $ do
  ret <- H.run options{debug=True} $ do
    tmp <- createTempFile

    H.writeFile tmp "ut"
    H.appendFile tmp "demir"

    H.readFile tmp

  assertEqual "" "utdemir" ret

test3 = testCase "shell" $ do
  (h1, h2) <- H.run options{debug=True} $ do
    tmp <- createTempFile

    H.writeFile tmp (B.pack [1..255])

    h1 <- takeWhile isHexDigit . C.unpack . fst <$> command_ "md5sum" [tmp] ""
    h2 <- takeWhile isHexDigit . C.unpack . fst <$> shell_ ("cat " ++ tmp ++ " | md5sum -") ""
    return (h1, h2)

  assertEqual "" h1 h2

test4 = testCase "exit" $ do
  (e1, e2) <- H.run options{debug=True} $ do
    (e1, _, _) <- command "grep" [] ""
    (e2, _, _) <- command "id" [] ""
    return (e1, e2)

  case (e1, e2) of
   (ExitFailure _, ExitSuccess) -> return ()
   _ -> assertFailure $ "Invalid return values: " ++ show (e1, e2)

main :: IO ()
main = defaultMain (testGroup "handsy" [test1, test2, test3, test4])
