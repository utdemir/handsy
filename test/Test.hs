{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           System.Handsy              as H

{- These aren't currently used in tests, but I
   import them for coverage reports. -}
import           System.Handsy.Remote       as H
import           System.Handsy.Util         as H

import           Control.Applicative
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char
import           Data.List
import           System.Exit

import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.TH

arbitraryBinary :: B.ByteString
arbitraryBinary = B.pack [1..255]

case_writeFile = do
  f <- H.run def $ do
    tmp <- mkTemp ""
    H.writeFile tmp arbitraryBinary
    return tmp
  ret <- B.readFile f
  assertEqual "" arbitraryBinary ret

case_readFile = do
  tmp <- H.run def $ mkTemp ""
  B.writeFile tmp arbitraryBinary
  ret <- H.run def $ H.readFile tmp
  assertEqual "" arbitraryBinary ret

case_appendFile = do
  ret <- H.run def $ do
    tmp <- mkTemp ""

    H.writeFile tmp "ut"
    H.appendFile tmp "demir"

    H.readFile tmp

  assertEqual "" "utdemir" ret

case_shell = do
  (h1, h2) <- H.run def $ do
    tmp <- mkTemp ""

    H.writeFile tmp (B.pack [1..255])

    h1 <- takeWhile isHexDigit . C.unpack . fst <$> command_ "md5sum" [tmp]
    h2 <- takeWhile isHexDigit . C.unpack . fst <$> shell_ ("cat " ++ tmp ++ " | md5sum -")
    return (h1, h2)

  assertEqual "" h1 h2

case_exit = do
  (e1, e2) <- H.run def $ do
    (e1, _, _) <- command "grep" []
    (e2, _, _) <- command "id" []
    return (e1, e2)

  case (e1, e2) of
    (ExitFailure _, ExitSuccess) -> return ()
    _ -> assertFailure $ "Invalid return values: " ++ show (e1, e2)

case_cwd = do
  (temp, pwd) <- H.run def $ do
    temp <- mkTempDir ""
    pwd:[] <- strLines . stdout <$> command_ "pwd" [] $~ def{cwd=temp}
    return (temp, pwd)
  assertEqual "" temp pwd


main :: IO ()
main = $(defaultMainGenerator)
