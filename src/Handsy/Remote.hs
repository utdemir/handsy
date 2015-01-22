{-# LANGUAGE OverloadedStrings #-}
module Handsy.Remote where

import           Prelude                hiding (readFile, writeFile)

import           Handsy

import qualified Data.ByteString.Char8  as B

import           Control.Monad.Free
import           Control.Monad.IO.Class

data RemoteOptions =
  RemoteOptions {
    sshCommand :: (String, [String])
  }

runRemote :: MonadIO m => RemoteOptions -> Handsy a -> m a
runRemote opts = iterM go
  where
    go :: (MonadIO m) => HandsyF (m a) -> m a
    go (Command prg args stdin next) = runSsh prg args stdin >>= next
    go (ReadFile fp next)      = do
      (_, stdin,  _) <- runSsh "cat" [fp] ""
      next stdin
    go (WriteFile fp str next) = do
      _ <- runSsh "dd" ["of=" ++ fp] str -- TODO: Escape necessary?
      next ()
    (ssh, sshOpts) = sshCommand opts
    runSsh prg args stdin = run (command ssh (sshOpts ++ prg : args) stdin)

-- Sample Program
remoteTest :: IO B.ByteString
remoteTest = runRemote (RemoteOptions ("ssh", ["root@utdemir.com"])) test
