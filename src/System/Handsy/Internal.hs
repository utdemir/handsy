{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module System.Handsy.Internal where

import           Control.Error
import           Control.Exception.Lifted  (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Operational
import           Control.Monad.Trans.Class
import qualified Data.ByteString.Lazy      as B
import           Data.Default.Class
import           System.Exit               (ExitCode)
import           System.IO                 (hPutStrLn, stderr)

type StdOut = B.ByteString
type StdErr = B.ByteString

data HandsyInstruction a where
 Shell :: String -> B.ByteString -> HandsyInstruction (ExitCode, StdOut, StdErr)

type Handsy a = ProgramT HandsyInstruction Script a

shellF :: FilePath -> B.ByteString -> Handsy (ExitCode, B.ByteString, B.ByteString)
shellF cmd stdin = singleton $ Shell cmd stdin

data Options =
  Options { debug :: Bool -- ^ Log commands to stderr before running
          }

instance Default Options where
  def = Options False

interpret :: forall r . forall a
           . Script r         -- ^ Acquire resource
          -> (r -> Script ()) -- ^ Release resource
          -> (r -> String -> B.ByteString -> Script (ExitCode, B.ByteString, B.ByteString))
          -> Options
          -> Handsy a
          -> EitherT String IO a
interpret acquire destroy f opts handsy = bracket acquire destroy (`go` handsy)
  where go :: r -> Handsy a -> Script a
        go res h = viewT h >>= \case
          Return x                   -> right x
          Shell cmdline stdin :>>= k -> when (debug opts) (liftIO $ hPutStrLn stderr cmdline)
                                        >> f res cmdline stdin >>= go res . k

interpretSimple :: (FilePath -> B.ByteString -> Script (ExitCode, B.ByteString, B.ByteString)) -- ^ 'readProcessWithExitCode'
                -> Options
                -> Handsy a
                -> Script a
interpretSimple f = interpret (return ()) (const (return ())) (const f)

handsyIO :: IO a -> Handsy a
handsyIO = lift . scriptIO

handsyLeft :: String -> Handsy a
handsyLeft = lift . left
