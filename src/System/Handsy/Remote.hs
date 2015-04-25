{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Remote where

import           Prelude                hiding (appendFile, readFile, writeFile)

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Monad
import           Control.Retry
import qualified Data.ByteString.Lazy   as B
import           Data.Default.Class
import           Data.Monoid
import           System.Exit
import           System.Handsy.Actions
import           System.Handsy.Internal
import           System.Handsy.Local

type Host = String
data SSHOptions =
  SSHOptions {
    -- | Path of `ssh` command
    sshPath        :: FilePath,

    -- | User
    sshUser        :: Maybe String,

    -- | Port to connect
    sshPort        :: Int,

    -- | Identity file to use
    identityFile   :: Maybe FilePath,

    -- | Connect timeout
    connectTimeout :: Int,

    {-| Whether to use control master for SSH connections.
        This significantly reduces execution time.
    -}
    controlMaster  :: Bool
  }

instance Default SSHOptions where
  def = SSHOptions "ssh" Nothing 22 Nothing 30 True

acquireCM :: Host -> SSHOptions -> Script FilePath
acquireCM host opts = do
  cm <- scriptIO (run def $ head . strLines . fst <$> command_ "mktemp" ["-u", "--suffix=.handsy"] def) >>= hoistEither

  let (ssh, params) = genSsh opts (Just cm)
  _ <- scriptIO . forkIO . void . run def . void $ command_ ssh (params ++ ["-M", "-N", host]) def
  scriptIO (waitForCM cm) >>= bool (left "Error establishing ControlMaster connection") (right ())

  return cm
  where
    checkCM :: FilePath -> IO Bool
    checkCM p = fmap (either (const False) id) . run def $ do
      let args = snd (genSsh opts Nothing) ++ ["-o", "ControlPath=" ++ p, "-O", "check"]
      command (sshPath opts) args def >>= return . \case
        (ExitSuccess, _, _) -> True
        _                   -> False
    waitForCM p = retrying (limitRetries 30) (\_ n -> return (not n)) (checkCM p)


releaseCM :: FilePath -> Script ()
releaseCM p = (scriptIO . run def{debug=False} $ void $ command_ "rm" ["-f", p] def) >>= hoistEither

genSsh :: SSHOptions -> Maybe FilePath -> (FilePath, [String])
genSsh opts cm = (sshPath opts,
                  [ "-p", show $ sshPort opts
                  , "-o", "ConnectTimeout=" ++ show (connectTimeout opts)]
                  <> maybe mempty (("-l":) . pure) (sshUser opts)
                  <> maybe mempty (("-i":) . pure) (identityFile opts)
                  <> maybe mempty (("-S":) . pure) cm
                 )

runSsh :: Host -> SSHOptions -> Maybe FilePath -> String -> B.ByteString
       -> Script (ExitCode, B.ByteString, B.ByteString)
runSsh host opts cm cmdline stdin' =
  let (ssh, params) = genSsh opts cm
  in  runS def{debug=False} $ command ssh (params ++ [host] ++ [cmdline]) def{stdin=stdin'}

-- | Executes the actions at a remote host
runRemote :: Options -> Host -> SSHOptions -> Handsy a -> IO (Either String a)
runRemote opts host sshOpts = runEitherT .
  case controlMaster sshOpts of
    False -> interpretSimple (runSsh host sshOpts Nothing) opts
    True -> interpret (acquireCM host sshOpts)
                       releaseCM
                       (runSsh host sshOpts . Just)
                       opts

-- | Copies a local file to remote host
pushFile :: FilePath -- ^ Local path of source
         -> FilePath -- ^ Remote path of destination
         -> Handsy ()
pushFile local remote = handsyIO (B.readFile local) >>= writeFile remote

-- | Fetches a file from remote host
pullFile :: FilePath -- ^ Remote path of source
         -> FilePath -- ^ Local path of destination
         -> Handsy ()
pullFile remote local = readFile remote >>= handsyIO . B.writeFile local
