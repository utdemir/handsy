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

-- | Waits specified number of seconds
sleep :: Int -> Handsy ()
sleep = liftIO . threadDelay . (* 1000000)

-- | Extract lines from a ByteString. Useful for parsing unix commands.
strLines :: C.ByteString -> [String]
strLines = lines . C.unpack

-- | Creates a temporary file
mkTemp :: String -> Handsy String
mkTemp suffix = head . strLines . fst
                  <$> command_ "mktemp" (bool ["-s", suffix] [] (null suffix)) ""

-- | Creates a temporary directory
mkTempDir :: String -> Handsy String
mkTempDir suffix = head . strLines . fst
                  <$> command_ "mktemp" ("-d" : bool ["-s", suffix] [] (null suffix)) ""

-- | Returns if the specified process is running. Uses `pidof`
isRunning :: String -> Handsy Bool
isRunning p = command "pidof" [p] "" >>= return . \case
  (ExitSuccess, _, _) -> True
  _                   -> False
