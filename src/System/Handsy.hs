{-# LANGUAGE OverloadedStrings #-}

module System.Handsy where

import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy           as B
import           System.Exit

import           Control.Monad.Free.TH
import           Control.Monad.Trans.Free
import           System.Process.ByteString.Lazy

import           System.Handsy.Internal         (HandsyF (..))
import qualified System.Handsy.Internal         as T

type Handsy = FreeT T.HandsyF IO

-- * Actions

-- | Runs a command
command :: FilePath     -- ^ Command to run
        -> [String]     -- ^ Arguments
        -> B.ByteString -- ^ Standart Input
        -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (status, stdout, stderr)
command = T.command

-- | Reads a file and returns the contents of the file.
readFile :: FilePath -> Handsy B.ByteString
readFile = T.readFile

-- | 'writeFile' @file str@ function writes the bytestring @str@, to the file @file@.
writeFile :: FilePath -> B.ByteString -> Handsy ()
writeFile = T.writeFile

-- | 'appendFile' @file str@ function appends the bytestring @str@, to the file @file@.
appendFile :: FilePath -> B.ByteString -> Handsy ()
appendFile = T.appendFile

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

-- | Executes the actions locally
run :: Handsy a -> IO a
run h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next)
      -> B.readFile fp >>= run . next
    Free (WriteFile fp str next)
      -> B.writeFile fp str >>= run . next
    Free (AppendFile fp str next)
      -> B.appendFile fp str >>= run . next
    Free (Command prg args stdin next)
      -> readProcessWithExitCode prg args stdin >>= run . next

