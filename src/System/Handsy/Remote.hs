{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Remote
  ( runRemote
  , Host

  -- * Options
  , SSHOptions (..)

  -- * Helpers
  , pushFile
  , pullFile

  -- * Re-exports
  , def
  ) where

import           Prelude                hiding (appendFile, readFile, writeFile)

import           System.Handsy
import           System.Handsy.Internal (interpret, interpretSimple)
import           System.Handsy.Util

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Control.Retry
import           Data.Bool
import qualified Data.ByteString.Lazy   as B

import           Control.Monad.IO.Class
import           Data.Default.Class

type Host = String
data SSHOptions =
  SSHOptions {
    -- | Path of `ssh` command
    sshPath       :: FilePath,

    -- | Port to connect
    sshPort       :: Int,

    {-| Whether to use control master for SSH connections.
        This significantly reduces execution time.
    -}
    controlMaster :: Bool
  }

instance Default SSHOptions where
  def = SSHOptions "ssh" 22 True


acquireCM :: Host -> SSHOptions -> IO FilePath
acquireCM host opts = do
  cm <- run def $ head . strLines . fst <$> command_ "mktemp" ["-u", "--suffix=.handsy"]

  let (ssh, params) = genSsh opts (Just cm)
  _ <- forkIO . run def . void $ command_ ssh (params ++ ["-M", "-N", host])
  bool (error "Error establishing ControlMaster connection") () <$> waitForCM cm

  return cm
  where
    checkCM :: FilePath -> IO Bool
    checkCM p = run def $ do
      let args = snd (genSsh opts Nothing) ++ ["-o", "ControlPath=" ++ p, "-O", "check"]
      command (sshPath opts) args >>= return . \case
        (ExitSuccess, _, _) -> True
        _                   -> False
    waitForCM p = retrying (limitRetries 30) (\_ n -> return (not n)) (checkCM p)


releaseCM :: FilePath -> IO ()
releaseCM p = run def{debug=False} $ void $ command_ "rm" ["-f", p]

genSsh :: SSHOptions -> Maybe FilePath -> (FilePath, [String])
genSsh opts cm = (sshPath opts, ["-p", show $ sshPort opts] ++ maybe [] (\i->["-S", i]) cm)

runSsh :: Host -> SSHOptions -> Maybe FilePath -> String -> B.ByteString
       -> IO (ExitCode, B.ByteString, B.ByteString)
runSsh host opts cm cmdline stdin' =
  let (ssh, params) = genSsh opts cm
  in  run def{debug=False} $ command ssh (params ++ [host] ++ [cmdline]) $~ def{stdin=stdin'}

-- | Executes the actions at a remote host
runRemote :: Options -> Host -> SSHOptions -> Handsy a -> IO a
runRemote opts host sshOpts =
  case controlMaster sshOpts of
    False -> interpretSimple (runSsh host sshOpts Nothing) opts
    True  -> interpret (acquireCM host sshOpts)
                       releaseCM
                       (runSsh host sshOpts . Just)
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
