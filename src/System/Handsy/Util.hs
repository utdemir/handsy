{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Util where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.List.Split
import           Prelude                    hiding (appendFile, readFile,
                                             writeFile)
import           System.Handsy

-- * Helpers for parsing return values of 'command' and 'shell'

class IsReturnValue a where
  stdout   :: a -> C.ByteString
  stderr   :: a -> C.ByteString
  exitCode :: a -> ExitCode

instance IsReturnValue (C.ByteString, C.ByteString) where
  stdout   = fst
  stderr   = snd
  exitCode = const ExitSuccess

instance IsReturnValue (ExitCode, C.ByteString, C.ByteString) where
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
strLines :: C.ByteString -> [String]
strLines = lines . C.unpack

-- * Frequently used functionality

-- | Waits specified number of seconds
sleep :: Int -> Handsy ()
sleep = liftIO . threadDelay . (* 1000000)

-- | Creates a temporary file
mkTemp :: String -> Handsy String
mkTemp suffix = head . strLines . fst
                  <$> command_ "mktemp" (bool ["--suffix=" ++ suffix] [] (null suffix)) def

-- | Creates a temporary directory
mkTempDir :: String -> Handsy String
mkTempDir suffix = head . strLines . fst
                  <$> command_ "mktemp" ("-d" : bool ["--suffix" ++ suffix] [] (null suffix)) def

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
  where parseOsRelease = fmap (filter $ not . flip elem "'\"") -- Hack to unquote
          <$> lookup "ID" . map ((\(x:xs) -> (x, concat xs)) . splitOn "=") . strLines
