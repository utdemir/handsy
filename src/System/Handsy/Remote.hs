{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Remote where

import           Prelude                hiding (appendFile, readFile, writeFile)

import           System.Handsy
import           System.Handsy.Core     (interpretSimple)

import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as B
import           System.Exit

import           Control.Monad.IO.Class

import           Text.ShellEscape

data RemoteOptions =
  RemoteOptions {
    -- | Path of `ssh` command and command line arguments
    sshCommand :: (FilePath, [String])
  }

-- | Executes the actions at a remote host
runRemote :: Options -> RemoteOptions -> Handsy a -> IO a
runRemote opts remote = interpretSimple runSsh opts
  where
    runSsh :: String -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString, B.ByteString)
    runSsh prg args stdin = let c = C8.unpack . C8.intercalate " " . map (bytes . bash . C8.pack) $ (prg:args)
                                (ssh, sshOpts) = sshCommand remote
                            in run options $ command ssh (sshOpts ++ [c]) stdin

-- | Copies a local file to remote host
pushFile :: FilePath -- ^ Local path of source
         -> FilePath -- ^ Remote path of destination
         -> Handsy ()
pushFile local remote = liftIO (B.readFile local) >>= writeFile remote

-- | Fetches a file from remote host
pullFile :: FilePath -- ^ Remote path of source
         -> FilePath -- ^ Local path of destination
         -> Handsy ()
pullFile remote local = readFile remote >>= liftIO . B.writeFile local
