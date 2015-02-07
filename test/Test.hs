{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.Handsy              as H
import           System.Handsy.Remote       as H

import           Control.Applicative
import qualified Data.ByteString.Lazy.Char8 as B
import           Data.Char
import           Data.List
import           System.Exit

import           Test.Tasty
import           Test.Tasty.HUnit

test1 = testCase "writeFile . readFile == id" $ do
  (h1, h2) <- H.run options{debug=True} $ do
    bin <- H.readFile "/bin/sh"
    tmp <- dropWhileEnd isSpace . B.unpack . fst <$> command_ "mktemp" [] ""

    H.writeFile tmp bin

    h1 <- takeWhile isHexDigit . B.unpack . fst <$> H.command_ "md5sum" ["/bin/sh"] ""
    h2 <- takeWhile isHexDigit . B.unpack . fst <$> H.command_ "md5sum" [tmp] ""

    return (h1, h2)

  assertEqual "" h1 h2

test2 = testCase "shell" $ do
  (h1, h2) <- H.run options{debug=True} $ do
    h1 <- takeWhile isHexDigit . B.unpack . fst <$> command_ "md5sum" ["/bin/sh"] ""
    h2 <- takeWhile isHexDigit . B.unpack . fst <$> shell_ "cat /bin/sh | md5sum -" ""
    return (h1, h2)

  assertEqual "" h1 h2

test3 = testCase "exit" $ do
  (e1, e2) <- H.run options{debug=True} $ do
    (e1, _, _) <- command "grep" [] ""
    (e2, _, _) <- command "id" [] ""
    return (e1, e2)

  case (e1, e2) of
   (ExitFailure _, ExitSuccess) -> return ()
   _ -> assertFailure $ "Invalid return values: " ++ show (e1, e2)

main :: IO ()
main = defaultMain (testGroup "handsy" [test1, test2, test3])
