{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Main where

import           Prelude                    hiding (appendFile, readFile,
                                             writeFile)

import           System.Handsy

import           Control.Applicative
import qualified Data.ByteString.Lazy       as B
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Char

import           Test.Tasty.HUnit
import           Test.Tasty.TH

arbitraryBinary :: B.ByteString
arbitraryBinary = B.pack [1..255]

case_writeFile = do
  f <- run def $ do
    tmp <- mkTemp ""
    writeFile tmp arbitraryBinary
    return tmp
  case f of
    Right fname -> B.readFile fname >>= assertEqual "" arbitraryBinary
    Left  err   -> assertFailure err

case_readFile = do
  run def (mkTemp "") >>= \case
    Left  err -> assertFailure err
    Right tmp -> do
      B.writeFile tmp arbitraryBinary
      run def (readFile tmp) >>= \case
        Left  err -> assertFailure err
        Right ret -> assertEqual "" arbitraryBinary ret

case_appendFile = do
  ret <- run def $ do
    tmp <- mkTemp ""

    writeFile tmp "ut"
    appendFile tmp "demir"

    readFile tmp

  either assertFailure (assertEqual "" "utdemir") ret

case_shell = do
  ret <- run def $ do
    tmp <- mkTemp ""

    writeFile tmp (B.pack [1..255])

    h1 <- takeWhile isHexDigit . C.unpack . fst <$> command_ "md5sum" [tmp] def
    h2 <- takeWhile isHexDigit . C.unpack . fst <$> shell_ ("cat " ++ tmp ++ " | md5sum -") def
    return (h1, h2)

  case ret of
    Left err       -> assertFailure err
    Right (h1, h2) -> assertEqual "" h1 h2

case_exit = do
  ret <- run def $ do
    (e1, _, _) <- command "grep" [] def
    (e2, _, _) <- command "id" [] def
    return (e1, e2)

  case ret of
    Right (ExitFailure _, ExitSuccess) -> return ()
    other -> assertFailure $ "Invalid return values: " ++ show other

case_cwd = do
  ret <- run def $ do
    temp <- mkTempDir ""
    pwd:[] <- strLines . stdout <$> command_ "pwd" [] def{cwd=temp}
    return (temp, pwd)

  case ret of
    Right (temp, pwd) -> assertEqual "" temp pwd
    other -> assertFailure $ "Invalid paths: " ++ show other

main :: IO ()
main = $(defaultMainGenerator)
