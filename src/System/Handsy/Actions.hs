{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Actions where

import           Control.Applicative
import           Control.Concurrent
import           Control.Error
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Char8      as B8
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Default.Class
import           Data.List.Split
import           Prelude                    hiding (appendFile, readFile,
                                             writeFile)
import           System.Exit
import           System.Handsy.Internal
import           Text.ShellEscape

-- | Runs a command
command :: FilePath     -- ^ Command to run
        -> [String]     -- ^ Arguments
        -> CommandOptions
        -> Handsy (ExitCode, BL.ByteString, BL.ByteString) -- ^ (status, stdout, stderr)
command cmd args opts = let cmd' = B8.unpack . B8.intercalate " " . map (bytes . bash . B8.pack) $ (cmd:args)
                        in  shell cmd' opts

{-| Executes the given string in shell. Example:

  > shell "ls" $~ def{cwd="/var/www"}
-}
shell :: String       -- ^ String to execute
      -> CommandOptions
      -> Handsy (ExitCode, BL.ByteString, BL.ByteString) -- ^ (ExitCode, Stdout, Stderr)
shell cmd opts = let esc = B8.unpack . bytes . bash . B8.pack
                     CommandOptions stdin' cwd' = opts
                 in  shellF ((if null cwd' then "" else ("cd " ++ esc cwd' ++ "; ")) ++ cmd) stdin'

data CommandOptions =
  CommandOptions { stdin :: BL.ByteString
                 , cwd   :: String
                 }

instance Default CommandOptions where
  def = CommandOptions "" ""

-- | Reads a file and returns the contents of the file.
readFile :: FilePath -> Handsy BL.ByteString
readFile fp = command "cat" [fp] def >>= \case
  (ExitSuccess, sout, _) -> return sout
  (_, _, serr)           -> lift . left $ "Serror reading " ++ fp ++ "\nSerr was: " ++ BL8.unpack serr

-- | @writeFile file str@ function writes the bytestring @str@, to the file @file@.
writeFile :: FilePath -> BL.ByteString -> Handsy ()
writeFile fp s = command "dd" ["of=" ++ fp] def{stdin=s} >>= \case
  (ExitSuccess, _, _) -> return ()
  (_, _, serr)      -> lift . left $ "Serror writing to " ++ fp  ++ "\nSerr was: " ++ BL8.unpack serr

-- | @appendFile file str@ function appends the bytestring @str@, to the file @file@.
appendFile :: FilePath -> BL.ByteString -> Handsy ()
appendFile fp s = command "dd" ["of=" ++ fp, "conv=notrunc", "oflag=append"] def{stdin=s} >>= \case
  (ExitSuccess, _, _) -> return ()
  (_, _, serr)      -> lift . left $ "Serror appending to " ++ fp ++ "\nSerr was: " ++ BL8.unpack serr

-- | Same as 'command', but ExitFailure ends the computation.
command_ :: FilePath -> [String] -> CommandOptions -> Handsy (BL.ByteString, BL.ByteString)
command_ path args opts = command path args opts >>= \case
  (ExitFailure code, _, serr) -> lift $ left ('`':path ++ ' ' : show args ++ "` returned " ++ show code
                                                ++ "\nSerr was: " ++ BL8.unpack serr)
  (ExitSuccess, sout, serr) -> return (sout, serr)

-- | Same as 'shell', but ExitFailure ends the computation.
shell_ :: String -> CommandOptions -> Handsy (BL.ByteString, BL.ByteString)
shell_ cmd opts = shell cmd opts >>= \case
  (ExitFailure code, _, serr) -> lift $ left ('`':cmd ++ "` returned " ++ show code
                                                ++ "\nSerr was: " ++ BL8.unpack serr)
  (ExitSuccess, sout, serr) -> return (sout, serr)

-- * Helpers for parsing return values of 'command' and 'shell'

class IsReturnValue a where
  stdout   :: a -> BL.ByteString
  stderr   :: a -> BL.ByteString
  exitCode :: a -> ExitCode

instance IsReturnValue (BL.ByteString, BL.ByteString) where
  stdout   = fst
  stderr   = snd
  exitCode = const ExitSuccess

instance IsReturnValue (ExitCode, BL.ByteString, BL.ByteString) where
  stdout   = \case (_, a, _) -> a
  stderr   = \case (_, _, a) -> a
  exitCode = \case (a, _, _) -> a

isSuccessful :: IsReturnValue a => a -> Bool
isSuccessful = isExitSuccess . exitCode

isExitSuccess :: ExitCode -> Bool
isExitSuccess = \case
  ExitSuccess   -> True
  ExitFailure _ -> False

-- | Extract lines from a ByteString. Useful for parsing unix commands.
strLines :: BL.ByteString -> [String]
strLines = lines . BL8.unpack

-- | Waits specified number of seconds
sleep :: Int -> Handsy ()
sleep = liftIO . threadDelay . (* 1000000)

-- | Creates a temporary file
mkTemp :: String -> Handsy String
mkTemp suffix = do
  out <- strLines .stdout <$> command_ "mktemp" (bool ["--suffix=" ++ suffix] [] (null suffix)) def
  lift $ tryHead ("weird output from mktemp: " ++ show out) out

-- | Creates a temporary directory
mkTempDir :: String -> Handsy String
mkTempDir suffix
  = command_ "mktemp" ("-d" : bool ["--suffix" ++ suffix] [] (null suffix)) def
    >>= lift . tryHead "weird output from mktemp" . strLines . stdout

-- | Returns if the specified process is running. Uses `pidof`
isRunning :: String -> Handsy Bool
isRunning p = isSuccessful <$> command "pidof" ["-s", "-x", p] def

data OS = NixOS | Debian | Ubuntu | RHEL | CentOS | Fedora | ArchLinux
  deriving (Show, Eq)

{-| Guesses the os using `/etc/os-release`. This currently only supports Linux distributions
    abiding systemd standards. -}
os :: Handsy (Maybe OS)
os = parseOsRelease <$> readFile "/etc/os-release" >>= return . \case
    Just "ubuntu" -> Just Ubuntu
    Just "debian" -> Just Debian
    Just "nixos"  -> Just NixOS
    Just "rhel"   -> Just RHEL
    Just "centos" -> Just CentOS
    Just "fedora" -> Just Fedora
    Just "arch"   -> Just ArchLinux
    _             -> Nothing
  where parseOsRelease = fmap (filter $ not . flip elem ['\'', '"']) -- Hack to unquote
          <$> lookup "ID" . map ((\(x:xs) -> (x, concat xs)) . splitOn "=") . strLines
