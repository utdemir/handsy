{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module System.Handsy.Util where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.IO.Class
import           Data.Bool
import qualified Data.ByteString.Lazy.Char8 as C
import           Prelude                    hiding (appendFile, readFile,
                                             writeFile)
import           System.Handsy

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

-- | Waits specified number of seconds
sleep :: Int -> Handsy ()
sleep = liftIO . threadDelay . (* 1000000)

-- | Extract lines from a ByteString. Useful for parsing unix commands.
strLines :: C.ByteString -> [String]
strLines = lines . C.unpack

-- | Creates a temporary file
mkTemp :: String -> Handsy String
mkTemp suffix = head . strLines . fst
                  <$> command_ "mktemp" (bool ["--suffix=" ++ suffix] [] (null suffix))

-- | Creates a temporary directory
mkTempDir :: String -> Handsy String
mkTempDir suffix = head . strLines . fst
                  <$> command_ "mktemp" ("-d" : bool ["-s", suffix] [] (null suffix))

-- | Returns if the specified process is running. Uses `pidof`
isRunning :: String -> Handsy Bool
isRunning p = command "pidof" ["-s", "-x", p] >>= return . \case
  (ExitSuccess, _, _) -> True
  _                   -> False
