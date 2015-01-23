{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Remote where

import           Prelude                  hiding (readFile, writeFile)

import           System.Handsy

import qualified Data.ByteString.Char8    as B

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Free

data RemoteOptions =
  RemoteOptions {
    sshCommand :: (String, [String])
  }

runRemote :: RemoteOptions -> Handsy a -> IO a
runRemote opts h = do
  x <- runFreeT h
  case x of
    Pure r -> return r
    Free (ReadFile fp next) -> do
      (_, stdin,  _) <- runSsh "cat" [fp] ""
      runRemote opts (next stdin)
    Free (WriteFile fp str next) -> do
      _ <- runSsh "dd" ["of=" ++ fp] str -- TODO: Escape necessary?
      runRemote opts (next ())
    Free (Command prg args stdin next) -> runSsh prg args stdin >>= run . next

  where
    (ssh, sshOpts) = sshCommand opts
    runSsh prg args stdin = run (command ssh (sshOpts ++ prg : args) stdin)

pushFile :: FilePath -> FilePath -> Handsy ()
pushFile local remote = liftIO (B.readFile local) >>= writeFile remote

pullFile :: FilePath -> FilePath -> Handsy ()
pullFile remote local = readFile remote >>= liftIO . B.writeFile local
