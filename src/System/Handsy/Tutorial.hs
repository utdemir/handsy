{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module System.Handsy.Tutorial (
    -- * Introduction
    -- $introduction

    -- * Examples
    -- $examples
    ) where

import           System.Handsy

{- $introduction
    @handsy@ is a small library mainly for applications which should make some
    operations on remote machines by SSH. It currently provides you:

    * A DSL describing basic system operations('command', 'readFile', 'writeFile' etc.)
    * Two interpreters for running this DSL locally or via SSH('run' and 'runRemote')
    * Some utility functions for common commands('os', 'mkTemp' etc.)


    If you're looking for a shell scripting alternative, look at @turtle@, @shelly@ or
    @shellmate@ packages. @handsy@ is mostly relevant for the ability to apply simple
    commands remotely.
-}

{- $examples
    Here is a simple demonstration:

@
import           Control.Applicative    ((\<$>))
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy   as B

import qualified System.Handsy          as H
import qualified System.Handsy.Remote   as H
import qualified System.Handsy.Util     as H

demo :: H.Handsy B.ByteString
demo = do
  os <- H.os

  -- We can do IO in Handsy monad
  liftIO . putStrLn $ "Hello from " ++ maybe \"UnknownOS\" show os ++ "!"

  tmpFile <- H.mkTemp "handsy"
  H.writeFile tmpFile "hello world!"
  H.stdout \<$\> H.command "cat" [tmpFile] H.def
@

    And now we can run it:

@
  runHere  = 'H.run' H.def demo >>= print
  runThere = 'H.runRemote' H.def "root@google.com" H.def{H.sshPort=2222} demo >>= print
@

> λ> runHere
> Hello from NixOS!
> "hello world!"
> λ> runThere
> Hello from CentOS!
> "hello world!"

    Internally, Handsy converts the DSL terms into series of shell commands and
    'run' and 'runRemote' functions describes how to apply those shell
    commands.

    You can see the given shell commands using 'debug' option:

> λ> H.run H.def{debug=True} demo >>= print
> cat /etc/os-release
> Hello from NixOS!
> mktemp $'--suffix=handsy'
> dd $'of=/run/user/1002/tmp.N3TlYC9Jfwhandsy'
> cat /run/user/1002/tmp.N3TlYC9Jfwhandsy
> "hello world!"



-}


