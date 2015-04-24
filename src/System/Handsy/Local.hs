module System.Handsy.Local where

import           Control.Error
import           System.Handsy.Internal
import           System.Process.ByteString.Lazy

runS :: Options -> Handsy a -> Script a
runS opts = interpretSimple (\cmdline -> scriptIO . readProcessWithExitCode "bash" ["-c", cmdline]) opts

-- | Executes the actions locally
run :: Options -> Handsy a -> IO (Either String a)
run opts = runEitherT . runS opts
