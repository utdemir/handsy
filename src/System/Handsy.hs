{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy
  ( Handsy
  , run

  -- * Commands
  , shell
  , command
  , readFile
  , writeFile
  , appendFile

  -- * Helpers
  , shell_
  , command_

  -- * Options
  , CommandOptions (..)
  , Options (..)

  -- * Re-exports
  , ExitCode (..)
  , def
  ) where

import           Prelude                        hiding (appendFile, readFile,
                                                 writeFile)

import           Data.Bool
import qualified Data.ByteString.Char8          as C8
import qualified Data.ByteString.Lazy           as B
import qualified Data.ByteString.Lazy.Char8     as C
import           System.Exit

import           Data.Default.Class
import           System.Process.ByteString.Lazy

import           Text.ShellEscape

import           System.Handsy.Internal         hiding (shell)
import qualified System.Handsy.Internal         as I

-- * Commands

-- | Runs a command
command :: FilePath     -- ^ Command to run
        -> [String]     -- ^ Arguments
        -> CommandOptions
        -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (status, stdout, stderr)
command cmd args opts = let cmd' = C8.unpack . C8.intercalate " " . map (bytes . bash . C8.pack) $ (cmd:args)
                        in  shell cmd' opts

{-| Executes the given string in shell. Example:

  > shell "ls" $~ def{cwd="/var/www"}
-}
shell :: String       -- ^ String to execute
      -> CommandOptions
      -> Handsy (ExitCode, B.ByteString, B.ByteString) -- ^ (ExitCode, Stdout, Stderr)
shell cmd opts = let esc = C8.unpack . bytes . bash . C8.pack
                     CommandOptions stdin' cwd' = opts
                 in  I.shell (bool ("cd " ++ esc cwd' ++ "; ") "" (null cwd') ++ cmd) stdin'

data CommandOptions =
  CommandOptions { stdin :: B.ByteString
                 , cwd   :: String
                 }
  deriving Show

instance Default CommandOptions where
  def = CommandOptions "" ""

-- | Reads a file and returns the contents of the file.
readFile :: FilePath -> Handsy B.ByteString
readFile fp = command "cat" [fp] def >>= \case
  (ExitSuccess, stdout, _) -> return stdout
  (_, _, stderr)           -> error $ "Error reading " ++ fp ++ "\nStderr was: " ++ C.unpack stderr

-- | @writeFile file str@ function writes the bytestring @str@, to the file @file@.
writeFile :: FilePath -> B.ByteString -> Handsy ()
writeFile fp s = command "dd" ["of=" ++ fp] def{stdin=s} >>= \case
  (ExitSuccess, _, _) -> return ()
  (_, _, stderr)      -> error $ "Error writing to " ++ fp  ++ "\nStderr was: " ++ C.unpack stderr

-- | @appendFile file str@ function appends the bytestring @str@, to the file @file@.
appendFile :: FilePath -> B.ByteString -> Handsy ()
appendFile fp s = command "dd" ["of=" ++ fp, "conv=notrunc", "oflag=append"] def{stdin=s} >>= \case
  (ExitSuccess, _, _) -> return ()
  (_, _, stderr)      -> error $ "Error appending to " ++ fp ++ "\nStderr was: " ++ C.unpack stderr

-- | Same as 'command', but ExitFailure is a runtime error.
command_ :: FilePath -> [String] -> CommandOptions -> Handsy (B.ByteString, B.ByteString)
command_ path args opts = command path args opts >>= \case
  (ExitFailure code, _, stderr) -> error ('`':path ++ ' ' : show args ++ "` returned " ++ show code
                                       ++ "\nStderr was: " ++ C.unpack stderr)
  (ExitSuccess, stdout, stderr) -> return (stdout, stderr)

-- | Same as 'shell', but ExitFailure is a runtime error.
shell_ :: String -> CommandOptions -> Handsy (B.ByteString, B.ByteString)
shell_ cmd opts = shell cmd opts >>= \case
  (ExitFailure code, _, stderr) -> error ('`':cmd ++ "` returned " ++ show code
                                       ++ "\nStderr was: " ++ C.unpack stderr)
  (ExitSuccess, stdout, stderr) -> return (stdout, stderr)

-- | Executes the actions locally
run :: Options -> Handsy a -> IO a
run = interpretSimple (\cmdline -> readProcessWithExitCode "bash" ["-c", cmdline])

