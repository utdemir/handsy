{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Handsy.Internal
  ( Handsy
  , interpret
  , interpretSimple
  , shell
  , Options (..)
  )
  where

import           Control.Exception         (bracket)
import           Control.Monad
import           Control.Monad.Operational
import qualified Data.ByteString.Lazy      as B
import           Data.Default.Class
import           System.Exit               (ExitCode)
import           System.IO                 (hPutStrLn, stderr)

type StdOut = B.ByteString
type StdErr = B.ByteString

data HandsyInstruction a where
 Shell :: String -> B.ByteString -> HandsyInstruction (ExitCode, StdOut, StdErr)

type Handsy a = ProgramT HandsyInstruction IO a

shell :: FilePath -> B.ByteString -> Handsy (ExitCode, B.ByteString, B.ByteString)
shell cmd stdin = singleton $ Shell cmd stdin

data Options =
  Options { debug :: Bool -- ^ Log commands to stderr before running
          }

instance Default Options where
  def = Options False

interpret :: forall r . forall a
           . IO r         -- ^ Acquire resource
          -> (r -> IO ()) -- ^ Release resource
          -> (r -> String -> B.ByteString
              -> IO (ExitCode, B.ByteString, B.ByteString))
          -> Options
          -> Handsy a
          -> IO a
interpret acquire destroy f opts handsy = bracket acquire destroy (`go` handsy)
  where go :: r -> Handsy a -> IO a
        go res h = viewT h >>= \case
          Return x                   -> return x
          Shell cmdline stdin :>>= k -> when (debug opts) (hPutStrLn stderr cmdline)
                                        >> f res cmdline stdin >>= go res . k

interpretSimple :: (FilePath -> B.ByteString
                    -> IO (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode'
                -> Options
                -> Handsy a
                -> IO a
interpretSimple f = interpret (return ()) (const (return ())) (const f)
