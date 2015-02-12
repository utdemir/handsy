{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Remote
  ( runRemote
  , remoteOptions
  , RemoteOptions(..)
  , pushFile
  , pullFile
  ) where

import           Prelude                hiding (appendFile, readFile, writeFile)

import           System.Handsy
import           System.Handsy.Core     (interpret, interpretSimple)
import           System.Handsy.Util

import           Control.Applicative
import           Control.Arrow
import           Control.Concurrent
import           Control.Monad
import           Control.Retry
import           Data.Bool
import qualified Data.ByteString.Char8  as C8
import qualified Data.ByteString.Lazy   as B

import           Control.Monad.IO.Class

import           Text.ShellEscape

data RemoteOptions =
  RemoteOptions {
    -- | Path of `ssh` command and command line arguments
    sshCommand    :: (FilePath, [String]),
    {-| Whether to use control master for SSH connections.
        This significantly reduces the execution time.
    |-}
    controlMaster :: Bool
  }

remoteOptions :: (FilePath, [String]) -> RemoteOptions
remoteOptions ssh = RemoteOptions ssh True

acquireCM :: (FilePath, [String]) -> IO FilePath
acquireCM (ssh, sshOpts) = do
  cm <- run options $ head . strLines . fst <$> command_ "mktemp" ["-u", "--suffix=.handsy"] ""

  _ <- forkIO . run options . void $ command_ ssh (["-M", "-S", cm, "-N"] ++ sshOpts) ""
  bool (error "Error establishing ControlMaster connection") () <$> waitForCM cm

  return cm
  where
    checkCM :: FilePath -> IO Bool
    checkCM p = run options $ command "ssh" ["-o", "ControlPath=" ++ p, "-O", "check", ssh] "" >>= return . \case
      (ExitSuccess, _, _) -> True
      _                   -> False
    waitForCM p = retrying (limitRetries 30) (\_ n -> return (not n)) (checkCM p)


releaseCM :: FilePath -> IO ()
releaseCM p = run options{debug=False} $ void $ command_ "rm" ["-f", p] ""

runSsh :: (FilePath, [String]) -> String -> [String] -> B.ByteString -> IO (ExitCode, B.ByteString, B.ByteString)
runSsh (ssh, sshOpts) prg args stdin =
  let c = C8.unpack . C8.intercalate " " . map (bytes . bash . C8.pack) $ (prg:args)
  in run options $ command ssh (sshOpts ++ [c]) stdin

-- | Executes the actions at a remote host
runRemote :: Options -> RemoteOptions -> Handsy a -> IO a
runRemote opts remote = case controlMaster remote of
  False -> interpretSimple (runSsh $ sshCommand remote) opts
  True  -> interpret (acquireCM (sshCommand remote))
                     releaseCM
                     (\i -> runSsh $ second (++ ["-S", i]) $ sshCommand remote)
                     opts

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
