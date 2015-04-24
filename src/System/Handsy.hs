{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy
  ( Handsy
  , handsyIO
  , handsyLeft

  -- * Interpreters
  , Options (..)

  -- ** Local
  , run

  -- ** Remote
  , runRemote
  , Host
  , SSHOptions (..)

  -- * Actions
  , CommandOptions (..)
  , command
  , shell
  , command_
  , shell_
  , readFile
  , writeFile
  , appendFile
  , sleep
  , mkTemp
  , mkTempDir
  , isRunning
  , os

  -- ** For remote actions
  , pushFile
  , pullFile

  -- * Utils
  , IsReturnValue
  , stdout
  , stderr
  , exitCode
  , isSuccessful
  , isExitSuccess
  , strLines

  -- * Re-exports
  , ExitCode (..)
  , def
  ) where

import           Prelude                hiding (appendFile, readFile, writeFile)

import           System.Handsy.Actions
import           System.Handsy.Internal
import           System.Handsy.Local
import           System.Handsy.Remote

import           Data.Default.Class
import           System.Exit
