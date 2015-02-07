{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy
  ( module System.Handsy
  , Handsy
  , Options
  , options
  , debug
  ) where

import qualified Data.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8     as C
import           System.Exit

import           Control.Monad.Trans.Free
import           System.Process.ByteString.Lazy

import           System.Handsy.Core             hiding (command)
import qualified System.Handsy.Core             as I

-- * Actions

-- | Runs a command
command :: FilePath     -- ^ Command to run
        -> [String]     -- ^ Arguments
        -> B.ByteString -- ^ Standart Input
        -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (status, stdout, stderr)
command = I.command

-- | Reads a file and returns the contents of the file.
readFile :: FilePath -> Handsy B.ByteString
readFile fp = command "cat" [fp] "" >>= \case
  (ExitSuccess, stdin, _) -> return stdin
  _                       -> error $ "Error reading " ++ fp

-- | @writeFile file str@ function writes the bytestring @str@, to the file @file@.
writeFile :: FilePath -> B.ByteString -> Handsy ()
writeFile fp s = command "dd" ["of=" ++ fp] s >>= \case
  (ExitSuccess, stdin, _) -> return ()
  _                       -> error $ "Error writing to " ++ fp

-- | @appendFile file str@ function appends the bytestring @str@, to the file @file@.
appendFile :: FilePath -> B.ByteString -> Handsy ()
appendFile fp s = command "dd" ["of=" ++ fp, "conv=notrunc", "oflag=append"] s >>= \case
  (ExitSuccess, stdin, _) -> return ()
  _                       -> error $ "Error appending to " ++ fp

-- * Helpers

{-| Executes the given string in shell

  > shell cmd stdin = command "/usr/bin/env" ["sh", "-c", cmd] stdin
-}
shell :: String       -- ^ String to execute
      -> B.ByteString -- ^ Standart input
      -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (ExitCode, Stdout, Stderr)
shell cmd stdin = command "/usr/bin/env" ["sh", "-c", cmd] stdin

-- | Same as 'shell', but ExitFailure is a runtime error.
shell_ :: String  -> B.ByteString -> Handsy (B.ByteString, B.ByteString)
shell_ cmd stdin = shell cmd stdin >>= \case
  (ExitFailure code, _, stderr) -> error ('`':cmd ++ "` returned " ++ show code
                                       ++ "\nStderr was: " ++ (C.unpack stderr))
  (ExitSuccess, stdout, stderr) -> return (stdout, stderr)

-- | Same as 'command', but ExitFailure is a runtime error.
command_ :: FilePath -> [String] -> B.ByteString -> Handsy (B.ByteString, B.ByteString)
command_ path args stdin = command path args stdin >>= \case
  (ExitFailure code, _, stderr) -> error ('`':path ++ ' ' :(show args) ++ "` returned " ++ show code
                                       ++ "\nStderr was: " ++ (C.unpack stderr))
  (ExitSuccess, stdout, stderr) -> return (stdout, stderr)

-- * Interpreter

run :: Options -> Handsy a -> IO a
run = interpretSimple readProcessWithExitCode
